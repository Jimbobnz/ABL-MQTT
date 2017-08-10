class mqttClient:


    DEFINE VARIABLE mqttSocket             AS HANDLE NO-UNDO.
    
    define static variable mqttCONNECT      as integer initial 1.
    define static variable mqttCONNACK      as integer initial 2.
    define static variable mqttPUBLISH      as integer initial 3.    
    define static variable mqttPUBACK       as integer initial 4.     
    define static variable mqttPUBREC       as integer initial 5.    
    define static variable mqttPUBREL       as integer initial 6.     
    define static variable mqttPUBCOMP      as integer initial 7.
    define static variable mqttSUBSCRIBE    as integer initial 8.    
    define static variable mqttSUBACK       as integer initial 9.    
    define static variable mqttUNSUBSCRIBE  as integer initial 10.     
    define static variable mqttUNSUBACK     as integer initial 11.         
    define static variable mqttPINGREQ      as integer initial 12.    
    define static variable mqttPINGRESP     as integer initial 13.   
    define static variable mqttDISCONNECT   as integer initial 14.    
    
    define private property username          as character no-undo GET. SET.
    define private property password          as character no-undo GET. SET.
    define public  property clientID          as character no-undo GET. SET.
    define private property cleanSession      as logical no-undo initial true GET. SET.
    define private property userData          as character  no-undo GET. SET.    
    
    define private property willTopic         as character  no-undo GET. SET.  
    define private property willMessage       as character  no-undo GET. SET.  
    
    define private property willRetain        as logical no-undo initial false GET. SET.    
    define private property willQos           as integer no-undo initial 0 GET. SET. 
    define private property willFlag          as logical no-undo initial false GET. SET. 
    
    define variable messageIdentifier         as integer no-undo initial 0.
    
    CONSTRUCTOR mqttClient(input clientID     as character, 
                           input cleanSession as logical,
                           input userData     as character  ):
                           
        CREATE SOCKET mqttSocket.
            
        /** Generate a random unique Client ID is one is not provided.**/            
        if length(clientID) = 0 then
        do:
            assign 
                clientID = substring(hex-encode(GENERATE-RANDOM-KEY),1,22).
                
            assign
                clientID = replace(clientID, '+', chr(random(65,90)))
                clientID = replace(clientID, '/', chr(random(65,90))).
        end.                
        
        /** Set the global property for clientID. **/        
        this-object:clientID = clientID.
        
    END CONSTRUCTOR.
    
    DESTRUCTOR mqttClient():
        if VALID-HANDLE(mqttSocket) then
        do:

            if mqttSocket:connected() then
                this-object:disconnect().

            delete object mqttSocket.
        end.
    end destructor.
    
    method integer bitWiseOR (INPUT X AS INTEGER, INPUT Y AS INTEGER):
        DEFINE VARIABLE b1 AS INTEGER NO-UNDO.
        DEFINE VARIABLE b2 AS INTEGER NO-UNDO.
        DEFINE VARIABLE n  AS INTEGER NO-UNDO.
        DEFINE VARIABLE Z  AS INTEGER NO-UNDO.
      
        DO n = 1 TO 32:
          ASSIGN
            b1 = GET-BITS(X, n, 1)
            b2 = GET-BITS(Y, n, 1)
            .
            IF b1 = 1 OR b2 = 1 THEN PUT-BITS(Z, n, 1) = 1.
        END.
      
        return Z.
    end method.
    
    method private integer leftShift (input x as integer , input y as integer ): 

        define variable i       as integer.
        define variable xtemp   as integer.
    
        do i = 1 to y:
            x = get-bits(x, 1, 31).
            put-bits(x, 2, 31) = x.
            put-bits(x, 1, 1) = 0. 
        end.
        
        return x.
    end method.    
    
    method private integer rightShift ( i_ivalue AS INTEGER, i_ishift AS INTEGER ):

        DEFINE VARIABLE iexp AS INT EXTENT 16 INITIAL [ 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768 ].

        RETURN INTEGER( TRUNCATE( i_ivalue / iexp[ i_ishift + 1], 0 ) ).

    end method.
    
    method private integer extent variableLengthEncoding (input x as integer ):
 
        define variable encodedByte              as integer          no-undo.
        define variable encodedBytes             as character        no-undo.
        define variable variableLengthEncoding  as integer extent   no-undo.
        define variable encodedByteRaw           as raw              no-undo.
        define variable i                       as integer          no-undo.
       
        do while x > 127 :
           
            encodedByte = bitWiseOR(input x, input 0x80).
     
            encodedBytes = encodedBytes + "," + string(encodedByte).
           
            x = this-object:rightShift(x,7).
           
        end.
       
        /** Handle the remaining bytes that fall under the 128 byte mark**/
        encodedByte = x.
        encodedBytes = encodedBytes + "," + string(encodedByte ).
        encodedBytes = left-trim(encodedBytes , ',').
        
        /** convert the array string into an actual integer array extent. **/
        extent(variableLengthEncoding) = num-entries(encodedBytes).
        
        do i = 1 to extent(variableLengthEncoding):
            variableLengthEncoding[i] = integer(entry(i, encodedBytes)).
        end.
       
        return variableLengthEncoding .
    end method.  
    
    method private integer packetSend(mqttPacket as memptr):
    
        /** Method returns number of bytes sent..**/

        if mqttSocket:type ne 'socket':U then
            return -1.
            
        if get-size(mqttPacket) eq 0 then
            message "No data to send."
                view-as alert-box error.            
            
        if mqttSocket:connected() and get-size(mqttPacket) gt 0 then 
            mqttSocket:write(mqttPacket , 1, get-size(mqttPacket )).                                          
            
        set-size(mqttPacket ) = 0.
        
        return mqttSocket:bytes-written.
    end method.
    
    method private memptr packetFixedHeader(input controlPacket   as integer, 
                                            input remainingLength as integer):
    
        define variable packetFixedHeader       as memptr no-undo.
        define variable remainingLengthEncoded  as integer extent no-undo.
        define variable i                       as integer no-undo.
        
        remainingLengthEncoded = this-object:variableLengthEncoding(remainingLength).
        
        set-byte-order(packetFixedHeader) = BIG-ENDIAN.
        
        set-size(packetFixedHeader) = 0.
        set-size(packetFixedHeader) = 1 + extent(remainingLengthEncoded).
        
        /** MQTT Packet Header Type.**/
        put-byte(packetFixedHeader, 1) = this-object:leftShift(controlPacket , 4).  
                 
        do i = 1 to extent(remainingLengthEncoded):
            PUT-BYTE(packetFixedHeader, 1 + i) = remainingLengthEncoded[i].
        end.
    
        return packetFixedHeader.       
        
    end method.
    
    method private memptr packetFixedHeader(input controlPacket   as integer,
                                            input DUPFlag         as logical,
                                            input QoSLevel        as integer,
                                            input REATIN          as logical, 
                                            input remainingLength as integer):
                                            
        define variable packetFixedHeader       as memptr no-undo.                                            
        define variable publishFlags            as integer no-undo initial 0. 
                                            
        packetFixedHeader = this-object:packetFixedHeader(controlPacket, remainingLength ).
        
        
        /** tac on some extra data to the first byte of the header flag**/
        
        publishFlags = get-byte(packetFixedHeader, 1 ).
        
        assign
            publishFlags = publishFlags + 8 when DUPFlag         
            publishFlags = publishFlags + 2 when QoSLevel eq 1       
            publishFlags = publishFlags + 4 when QoSLevel eq 2
            publishFlags = publishFlags + 1 when REATIN.          
            
        put-byte(packetFixedHeader, 1 ) = publishFlags.            
                                    
        
        return packetFixedHeader.       
                                            
    end method.                                                
    
    method private memptr packetVariableHeader(input protocolLevel as integer,
                                               input connectFlag   as integer,
                                               input keepAlive     as integer): 
    
        define variable packetVariableHeader       as memptr no-undo.
        define variable protocolName               as character initial "MQTT":U no-undo.
        
        set-byte-order(packetVariableHeader) = BIG-ENDIAN.
        
        set-size(packetVariableHeader) = 0.
        set-size(packetVariableHeader) = 10.      
        
        put-unsigned-short( packetVariableHeader , 1 ) = length(protocolName , "RAW").
        put-string ( packetVariableHeader , 3, length(protocolName,"RAW") ) = protocolName .
        
        /** Figure 3.3 - Protocol Level byte, single byte **/
        put-byte(packetVariableHeader , 7) = protocolLevel .
        
        /** Figure 3.4 - Connect Flag bits, single byte **/
        put-byte(packetVariableHeader , 8) = connectFlag.
        
        /** Figure 3.5 Keep Alive bytes (16 bits / 2 bytes) **/
        put-unsigned-short(packetVariableHeader , 9) = keepAlive.
    
        return packetVariableHeader .       
        
    end method.  
    
    method private memptr packetVariableHeader(input topicName        as character,
                                               input packetIdentifier as integer):

        &scoped-define controlLength 2
                                                       
        define variable packetVariableHeader        as memptr  no-undo.
        define variable packetVariableHeaderSize    as integer no-undo.
        
        set-byte-order(packetVariableHeader) = BIG-ENDIAN.
        
        assign        
            packetVariableHeaderSize = {&controlLength} + length(topicName, "RAW")
            packetVariableHeaderSize = packetVariableHeaderSize + 2 when (packetIdentifier > 0).  
        
        set-size(packetVariableHeader) = 0.
        set-size(packetVariableHeader) = packetVariableHeaderSize.
        
        put-unsigned-short( packetVariableHeader, 1) = length(topicName, "RAW").
        put-string ( packetVariableHeader, 3, length(topicName,"RAW")) = topicName.
        
        if logical(packetIdentifier) then
            put-unsigned-short( packetVariableHeader , {&controlLength} + length(topicName, "RAW") + 1) = packetIdentifier.
          
        return packetVariableHeader.
            
    end method.
    
    method private memptr packetConnectPayload ():
    
        define variable mqttPayloadSize as integer initial 0 no-undo.
        
        define variable mqttPayload as memptr.
        
        set-byte-order(mqttPayload) = BIG-ENDIAN.
        
        &scoped-define controlLength 2
        
        /** Calculate the payload size. **/
        
       assign 
            mqttPayloadSize = mqttPayloadSize + {&controlLength} + length(this-object:clientID,"raw")       when this-object:clientID       ne ""
            mqttPayloadSize = mqttPayloadSize + {&controlLength} + length(this-object:willTopic,"raw")      when this-object:willTopic      ne "" and this-object:willFlag
            mqttPayloadSize = mqttPayloadSize + {&controlLength} + length(this-object:willMessage ,"raw")   when this-object:willMessage    ne "" and this-object:willFlag
            mqttPayloadSize = mqttPayloadSize + {&controlLength} + length(this-object:username ,"raw")      when this-object:username       ne "" 
            mqttPayloadSize = mqttPayloadSize + {&controlLength} + length(this-object:password ,"raw")      when this-object:password       ne "".
    
    
        set-size(mqttPayload) = 0.
        set-size(mqttPayload) = mqttPayloadSize.
      
        define variable mqttPayloadPos as integer initial 1.
        
         if this-object:clientID ne "" then do: 
            PUT-UNSIGNED-SHORT (mqttPayload , mqttPayloadPos ) = length(this-object:clientID , "raw" ). 
            mqttPayloadPos = mqttPayloadPos + {&controlLength}. 
            PUT-STRING(mqttPayload , mqttPayloadPos, length(this-object:clientID , "raw" ) ) = this-object:clientID. 
            mqttPayloadPos = mqttPayloadPos + length(clientID , "raw" ).
        end. 
    
         if this-object:willTopic ne "" and this-object:willFlag then 
         do: 
            PUT-UNSIGNED-SHORT ( mqttPayload , mqttPayloadPos ) = length(this-object:willTopic , "raw" ).
            mqttPayloadPos = mqttPayloadPos + {&controlLength}.   
            PUT-STRING(mqttPayload , mqttPayloadPos, length(this-object:willTopic , "raw" ) ) = this-object:willTopic .
            mqttPayloadPos = mqttPayloadPos + length(this-object:willTopic , "raw" ). 
         end. 
    
         if this-object:willMessage ne "" and this-object:willFlag then 
         do: 
            PUT-UNSIGNED-SHORT ( mqttPayload , mqttPayloadPos ) = length(this-object:willMessage , "raw" ).
            mqttPayloadPos = mqttPayloadPos + {&controlLength}.   
            PUT-STRING(mqttPayload , mqttPayloadPos , length(this-object:willMessage , "raw" ) ) = this-object:willMessage .
            mqttPayloadPos = mqttPayloadPos + length(this-object:willMessage , "raw" ).   
      end.

      if this-object:username ne "" then 
      do: 
      
          PUT-UNSIGNED-SHORT ( mqttPayload , mqttPayloadPos ) = length(this-object:username , "raw" ).
          mqttPayloadPos = mqttPayloadPos + {&controlLength}.
          PUT-STRING(mqttPayload , mqttPayloadPos, length(this-object:username , "raw" ) ) = this-object:username .
          mqttPayloadPos = mqttPayloadPos + length(this-object:username , "raw" ).
      end.

      if this-object:password ne "" then 
      do: 
          PUT-UNSIGNED-SHORT ( mqttPayload , mqttPayloadPos ) = length(this-object:password , "raw" ).
          mqttPayloadPos = mqttPayloadPos + {&controlLength}.
          PUT-STRING(mqttPayload , mqttPayloadPos, length(this-object:password , "raw" ) ) = this-object:password .
      end. 
        
      return mqttPayload.
        
    end method.
    
    method private memptr packetPayloadPublish (input topicMessage as character):
    
        define variable mqttPayload as memptr.
        
        set-byte-order(mqttPayload) = BIG-ENDIAN.
        
        set-size(mqttPayload) = 0.
        set-size(mqttPayload) = length(topicMessage , "raw").
        
        if get-size(mqttPayload) > 0 then
            put-string(mqttPayload,1, length(topicMessage , "raw")) = topicMessage.  
            
        return mqttPayload .            
    end.
  
  method private memptr packetBuilder(input mqttFixedHeader    as memptr,
                                      input mqttVariableHeader as memptr,
                                      input mqttPayload        as memptr):
                              
            
      define variable mqttCompletePacket as memptr no-undo. 
      
      set-size(mqttCompletePacket ) = 0 .
      
      set-size(mqttCompletePacket ) = get-size(mqttFixedHeader) + 
                                      get-size(mqttVariableHeader) + 
                                      get-size(mqttPayload).
                                      
      copy-lob from object mqttFixedHeader    to object mqttCompletePacket overlay at 1.    
      copy-lob from object mqttVariableHeader to object mqttCompletePacket overlay at get-size(mqttFixedHeader) + 1.                             
      copy-lob from object mqttPayload        to object mqttCompletePacket overlay at get-size(mqttFixedHeader) + get-size(mqttVariableHeader ) + 1.      
             
      return mqttCompletePacket.                               
  end method.                                
    
    /** Public Methods**/    
    
    method void max_inflight_messages_set():
    end method.    

    method void max_queued_messages_set():
    end method.
    
    method void message_retry_set():
    end method.    
    
    method void enable_logger():
    end method.      

    method void disable_logger():
    end method.            
    
    method void username_pw_set(input username as character, 
                                input password as character):
                                
        this-object:username = username.                   
        this-object:password = password.
                               
    end method.         

    method void user_data_set():
    end method.  
    
    method void will_set(input willTopic    as character,
                         input willMessage  as character,
                         input willQos      as integer,
                         input willRetain   as logical):
                         
        this-object:willTopic   = willTopic.                         
        this-object:willMessage = willMessage.                         
        this-object:willQos     = willQos.     
        this-object:willRetain  = willRetain.                      
        
        if length(willTopic)   > 0 and 
           length(willMessage) > 0 then        
           do:
                this-object:willFlag = true. 

           end.                
        else
        do:
            this-object:willFlag    = false. 
            this-object:willRetain  = false.
            this-object:willQos     = 0.
        end.        
        
    end method.      
    
    method private integer decodeCONNACKVariableHeader(input variableHeaderPacket as memptr):

        define variable connectAcknowledgeFlags as integer no-undo.
            
        if get-size(variableHeaderPacket ) eq 2 then 
        do:
            /** get the Session Present Flag from the first bit of the first byte.**/     
            connectAcknowledgeFlags = get-byte(variableHeaderPacket,1).
            message "Session Present Flag" integer(get-bits(connectAcknowledgeFlags, 1, 1)). 
                
            /** return the second byte. **/    
            return get-byte(variableHeaderPacket , 2). 
        
        end.
        else
            return ?.
    
    end method.

    method logical connect(input connectionString as character):
    
        &SCOPED-DEFINE protocolLevelMQTT311 4
        
        /**CONNECT 1 Client to Server Client request to connect to Server **/
        
        
        define variable packetFixedHeader       as memptr no-undo.
        define variable packetVariableHeader    as memptr no-undo.
        define variable packetPayload           as memptr no-undo.
        define variable mqttPacket              as memptr no-undo.
        
        set-byte-order(mqttPacket)              = BIG-ENDIAN.              
        set-byte-order(packetPayload)           = BIG-ENDIAN.
        set-byte-order(packetVariableHeader)    = BIG-ENDIAN.
        set-byte-order(packetFixedHeader)       = BIG-ENDIAN.
        
        define variable connectFlag as integer initial 0.
        
        assign    
            connectFlag = connectFlag + 128     when ( length(this-object:username) > 0 )
            connectFlag = connectFlag + 64      when ( length(this-object:username) > 0 and length(this-object:password) > 0 )
            connectFlag = connectFlag + 32      when this-object:willRetain and this-object:willFlag  
            
            connectFlag = connectFlag + 16      when this-object:willQos eq 2 and this-object:willFlag   
            connectFlag = connectFlag + 8       when this-object:willQos eq 1 and this-object:willFlag
            
            connectFlag = connectFlag + 4       when this-object:willFlag and this-object:willRetain   
            connectFlag = connectFlag + 2       when this-object:cleanSession
            .
            
        /** packetPayload **/
        packetPayload = packetConnectPayload().
        
            
        /** packetVariableHeader **/
        packetVariableHeader = this-object:packetVariableHeader(input {&protocolLevelMQTT311}, 
                                                                input connectFlag,
                                                                input 60).   /** 60 seconds , need to do a ping in this time frame. **/
                                                    
 /*         message get-size(packetVariableHeader ) skip */
/*                  get-string(packetVariableHeader ,1) */
/*                  view-as alert-box info title "packetVariableHeader ". */
        
        /** packetFixedHeader **/
        

        
        packetFixedHeader = this-object:packetFixedHeader(input mqttCONNECT,
                                                          input integer(get-size(packetVariableHeader) + get-size(packetPayload))
                                                          ).               
                                                          
/*    */
/*           message get-size(packetFixedHeader ) skip */
/*                  get-string(packetFixedHeader ,1) */
/*                  view-as alert-box info title "packetFixedHeader ". */
        
        /** packetPayload **/
        
        packetPayload = packetConnectPayload().
        
        message "packetFixedHeader" get-size(packetFixedHeader).
        message "packetVariableHeader" get-size(packetVariableHeader).        
        message "packetPayload" get-size(packetPayload).
        
        /** packetBuilder **/
        mqttPacket = this-object:packetBuilder(packetFixedHeader,
                                               packetVariableHeader,
                                               packetPayload).
                                   
        /** Connect the MQTT server **/                          
        mqttSocket:connect(connectionString ).   
        
        copy-lob from object mqttPacket to file "./mqttConnectPacket.txt".
        
        this-object:packetSend(input  mqttPacket ).
        
        /* this-object:waitForServer(). */     
        
        define variable packetLength        as integer no-undo.
        define variable timethen            as datetime no-undo.
        define variable mqttServerData      as memptr no-undo.
        define variable returnCodeResponce  as integer no-undo.
        define variable remainingLength     as integer no-undo.
        
        timethen = now.

        WAIT-FOR-SERVER:
        do while mqttSocket:connected():
        
            if absolute( interval(timethen, now, 'seconds') ) ge 10 then
                leave WAIT-FOR-SERVER.
        
            packetLength = mqttSocket:get-bytes-available().
            
            message "packet size: " packetLength.
        
            if logical(packetLength) then
            do:
                set-size(mqttServerData ) = 0.  
                set-size(mqttServerData ) = packetLength .      
                
                if mqttSocket:read(mqttServerData , 1, packetLength , READ-EXACT-NUM ) then
                do:
                
                    if not logical(mqttSocket:BYTES-READ) then 
                    do:
                    
                        message "Timed out waiting for MQTT server to respond."
                            view-as alert-box error.
                            
                        leave WAIT-FOR-SERVER.
                    end.    
                    
                    if get-byte(mqttServerData, 1) eq this-object:leftShift(mqttCONNACK, 4) then
                    do:
                    
                        message "Connection acknowledged!".
                        
                        remainingLength = get-byte(mqttServerData, 2).
                        
                        if remainingLength eq 2 then
                        do: 
                        
                            define variable CONNACKVariableHeader as memptr no-undo.
                            
                            set-size(CONNACKVariableHeader) = 0.
                            set-size(CONNACKVariableHeader) = remainingLength .
                            
                            copy-lob from object mqttServerData starting at 3 for remainingLength to CONNACKVariableHeader.
                                                
                            returnCodeResponce = this-object:decodeCONNACKVariableHeader(input CONNACKVariableHeader ).
                                
                            case returnCodeResponce:
                                when 0 then
                                    message "Connection accepted".
                                when 1 then 
                                    message "The Server does not support the level of the MQTT protocol requested by the Client".
                                when 2 then
                                    message "The Client identifier is correct UTF-8 but not allowed by the Server".
                                when 3 then 
                                    message "The Network Connection has been made but the MQTT service is unavailable".
                                when 4 then
                                    message "The data in the user name or password is malformed".
                                when 5 then 
                                    message "The Client is not authorized to connect".                                                                                                        
                                otherwise
                                    message substitute("&1, Reserved for future use", returnCodeResponce).
                            end case.    
                        end.                            
                    end. 
                end.
               
               leave WAIT-FOR-SERVER.
            end.
        end.
        
        message "MQTT broker contected? " mqttSocket:connected().
        
    end method.
    
    method logical connect():
    
        define variable connectionString as character no-undo.
        
        /** default the MQTT broker/server as being on the localserver.**/
        connectionString = "-H localhost -P 1883":U.
        
        return this-object:connect(connectionString).
    end method.
    
    method logical PUBLISH( input topic         as character, 
                            input topicMessage  as character,
                            input QoSLevel      as integer,
                            input retainFlag    as logical):
               
        define variable packetFixedHeader       as memptr no-undo.
        define variable packetVariableHeader    as memptr no-undo.
        define variable packetPayload           as memptr no-undo.
        define variable mqttPacket              as memptr no-undo.
        
        set-byte-order(packetFixedHeader)       = BIG-ENDIAN.
        set-byte-order(packetVariableHeader)    = BIG-ENDIAN.
        set-byte-order(packetPayload)           = BIG-ENDIAN.                
        set-byte-order(mqttPacket)              = BIG-ENDIAN.        
        
        define variable duplicateFlag           as logical no-undo initial false.
        
        /** If the QoS level is set high than zero, a message identifier is required. **/
        
        
        
        Assign
            messageIdentifier = messageIdentifier + 1 when logical(QoSLevel). 
            
        message "topicMessage"  topicMessage.    
        
        packetPayload = this-object:packetPayloadPublish(input topicMessage).   
        
        message "PUBLISH packetPayload" get-size(packetPayload).        
                
        packetVariableHeader = this-object:packetVariableHeader(input topic, 
                                                                input messageIdentifier).
                                                                
        message "PUBLISH packetVariableHeader" get-size(packetVariableHeader).                                                                
        
        packetFixedHeader = this-object:packetFixedHeader(input mqttPUBLISH, /** 3 LSH 4 **/
                                                          input duplicateFlag,
                                                          input QoSLevel,       
                                                          input retainFlag,  
                                                          input integer(get-size(packetVariableHeader) + get-size(packetPayload))
                                                          ).  
                                                          
        message "PUBLISH packetFixedHeader" get-size(packetFixedHeader).                                                          
                                                          
        /** packetBuilder **/
        mqttPacket = this-object:packetBuilder(packetFixedHeader,
                                               packetVariableHeader,
                                               packetPayload).
                                   
        copy-lob from object mqttPacket to file "./mqttPublishPacket.txt".
        
        this-object:packetSend(input mqttPacket ).
        
        return true.               
                            
    end method.                            

    method logical PUBLISH( input topic         as character, 
                            input topicMessage  as character):
                            
        return this-object:publish(input topic, input topicMessage, input 0, input false).                            
        
    end method.    
    
    method logical SUBSCRIBE():
    
        /** SUBSCRIBE 8 Client to Server Client subscribe request**/
    
    end method.
    
    method logical UNSUBSCRIBE():
    
        /** SUBSCRIBE 10 Client to Server Client subscribe request**/
    
    end method.    
    
    method logical PINGREQ():
    
        /** SUBSCRIBE 12 **/
    
    end method.      
    
    method void DISCONNECT():
    
        /** 14 LSB 4 **/
        
        define variable packetFixedHeader as memptr no-undo.
        
        /** zero payload for disconnect statments **/
        
        packetFixedHeader = this-object:packetFixedHeader(mqttDISCONNECT, 0).   
                                              
        this-object:packetSend(packetFixedHeader).                                                      
        
        /**
            Don't need to wait for a responce from the MQTT server.
        **/
        
        if mqttSocket:connected() then 
            mqttSocket:disconnect().             
    
    end method.      

end class.
