module Main where

import Language.Atom
import Language.Atom.Unit
import GHC.Word


main :: IO ()
main = do 
  (schedule, _, _, _, _) <- compile "sonar_example" atomCfg sonar
  putStrLn $ reportSchedule schedule

atomCfg :: Config
atomCfg = defaults { cFuncName = "atom_tick"
                   , cStateName = "state_sonar"
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
            , "void beep_on(void);"
            , "void beep_off(void);"
            , "void sonar_trigger(void);"
            ]

  , unlines [ "int main(void) {"
            , "    while (true) {"
            , "        atom_tick();"
            , "        usleep(1000);"
            , "    }"
            , "    return 0;"
            , "}"

            , "void beep_on(void) {"
            , "    printf(\"%lu: beep_on()\\n\", __global_clock);"
            , "}"
            , ""
            , "void beep_off(void) {"
            , "    printf(\"%lu: beep_off()\\n\", __global_clock);"
            , "}"
            , ""


            , "void sonar_trigger(void) {"
            , "    if (rand() % 4) {"
            , "        g_sensor_value = rand();"
            , "        g_sensor_ready = true;"
            , "        printf(\"%lu: Triggered sonar, value=%u\\n\","
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

sonar :: Atom ()
sonar = do

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
  beepOn <- bool "beep_on" False

  period 3000 $ phase 000 $ atom "beepOn" $ do
    call "beep_on"
    beepOn <== true
    startTimer warmup $ Const 10
  
  period 3000 $ phase 050 $ atom "beepOff" $ do
    call "beep_off"
    beepOn <== false


