module DSL where

-- deep

-- equiv to list without stop:
-- data RobotProg = Instr RobotInstrs RobotProg | Stop
-- data RobotProg = Instr RobotInstrs RobotProg | Stop | Dance -- not equiv
-- data RobotInstrs = Dance
--     | Teleport Int Int
--     | Say String

data RobotLang
    = Stop -- not RobotLang here cos stop stops!
    | Dance RobotLang
    | Teleport Int Int RobotLang
    | Say String RobotLang
    -- | Sequence RobotLang RobotLang
               -- ^ what if stop is here?

-- type RobotProgram = [RobotLang] -- what happens if stop is in the middle?

exampleProgram :: RobotLang
exampleProgram = Teleport 10 10
                $ Say "hello"
                $ Dance
                $ Stop

talkLog :: RobotLang -> [String]
talkLog (Say s rl) = s : talkLog rl
talkLog Stop = []
talkLog (Dance rl) = talkLog rl
talkLog (Teleport _ _ rl) = talkLog rl

-- howManyDances :: RobotLang -> Int