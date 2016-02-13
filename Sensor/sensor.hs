module Main where

import Language.Atom
import Language.Atom.Unit
import GHC.Word
 
main :: IO ()
main = do
   (sched, _, _, _, _) <- compile "atom_example" atomCfg example
   putStrLn $ reportSchedule sched

atomCfg :: Config
atomCfg = defaults { cFuncName = "atom_tick"
                   , cStateName = "state_example"
                   , cCode = prePostCode
                   , hCode = prePostHeader
                   , cRuleCoverage = False
                   }

prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
prePostCode _ _ _ =
  ( unlines [ "// ---- This source is automatically generated by Atom ----"
            , "#include <stdlib.h>"
            , "#include <stdio.h>"
            , "#include <unistd.h>"
            , ""
            , "bool g_sensor_ready;"
            , "uint16_t g_sensor_value;"
            , "void sensor_on(void);"
            , "void sensor_off(void);"
            , "void sensor_trigger(void);"
            ]

  , unlines [ "int main(void) {"
            , "    while (true) {"
            , "        atom_tick();"
            , "        usleep(1000);"
            , "    }"
            , "    return 0;"
            , "}"

            , "void sensor_on(void) {"
            , "    printf(\"%lu: sensor_on()\\n\", __global_clock);"
            , "}"
            , ""
            , "void sensor_off(void) {"
            , "    printf(\"%lu: sensor_off()\\n\", __global_clock);"
            , "}"
            , ""


            , "void sensor_trigger(void) {"
            , "    if (rand() % 4) {"
            , "        g_sensor_value = rand();"
            , "        g_sensor_ready = true;"
            , "        printf(\"%lu: Triggered sensor, value=%u\\n\","
            , "               __global_clock, g_sensor_value);"
            , "    }"
            , "}"
            , ""
            , "// ---- End automatically-generated source ----"
            ])

prePostHeader :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
prePostHeader _ _ _ =
  ( unlines [ "// ---- This header is automatically generated by Atom ----"
            ]
  , unlines [ "// ---- End automatically-generated header ----"
            ])

example :: Atom ()
example = do

  clock <- tickSecond

  checkSensor 40000 $ do
    printStrLn "Sensor value over threshold!"

tickSecond :: Atom (V Word64)
tickSecond = do
  clock <- word64 "clock_sec" 0
  period 1000 $ exactPhase 0 $ atom "second" $ incr clock
  return clock


checkSensor :: Word16 -> Atom () -> Atom ()
checkSensor threshold overThresholdAction = atom "check_sensor" $ do
  ready <- return $ bool' "g_sensor_ready"
  sensorValue <- return $ word16' "g_sensor_value"
  warmup <- timer "warmup"
  triggered <- bool "triggered" False
  sensorOn <- bool "sensor_on" False

  period 2000 $ phase 500 $ atom "powerOn" $ do
    call "sensor_on"
    triggered <== false
    ready <== false
    sensorOn <== true
    startTimer warmup $ Const 10
  
  atom "trigger" $ do
    cond $ timerDone warmup &&. not_ (value triggered) &&. value sensorOn
    triggered <== true
    call "sensor_trigger"
    
  atom "checkSensorValue" $ do
    cond $ value ready
    ready <== false
    sensorOn <== false
    call "sensor_off"
    atom "checkThreshold" $ do
      cond $ value sensorValue >. Const threshold
      overThresholdAction
  
  period 2000 $ phase 550 $ atom "powerOff" $ do
    cond $ value sensorOn
    ready <== false
    printStrLn "Sensor timeout."
    call "sensor_off"