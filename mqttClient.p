
define variable mqttClient as class mqttClient.

etime(true).

mqttClient = NEW mqttClient("", true, "").

/**mqttClient:will_set("ABC/MQT", "DIED", 0, true).**/

mqttClient:connect("-H flow -S 1883").

mqttClient:disconnect().


