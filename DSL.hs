module DSL where

-- deep
-- we actually went to the deep effort of making a full data type for our lang
-- syntax is data type (deep)
-- haskell is great for this! (rich data types)

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
    | Sing String RobotLang
    -- | Sequence RobotLang RobotLang
               -- ^ what if stop is here?

-- type RobotProgram = [RobotLang] -- what happens if stop is in the middle?

exampleProgram :: RobotLang
exampleProgram = Teleport 10 10
                $ Say "hello"
                $ Sing "hello"
                $ Dance
                $ Stop

talkLog :: RobotLang -> [String]
talkLog (Say s rl) = s : talkLog rl -- talkLog rl `snoc` s
talkLog (Sing s rl) = s : talkLog rl
talkLog Stop = []
talkLog (Dance rl) = talkLog rl
talkLog (Teleport _ _ rl) = talkLog rl

howManyDances :: RobotLang -> Int
howManyDances (Dance rl) = 1 + howManyDances rl
howManyDances Stop       = 0
howManyDances (Teleport _ _ rl) = howManyDances rl
howManyDances (Say _ rl) = howManyDances rl
howManyDances (Sing _ rl) = howManyDances rl

-- new sem
both :: RobotLang -> ([String], Int)
both x = (talkLog x, howManyDances x)

-- shallow
-- so called cos not much on top of the host, just a bunch of functions
-- syntax is just Haskell
-- prototyping

type DancesDanced = Int

stop :: DancesDanced
stop = 0

dance :: DancesDanced -> DancesDanced
dance rl = 1 + rl

teleport :: Int -> Int -> DancesDanced -> DancesDanced
teleport _ _ x = x

say :: String -> DancesDanced -> DancesDanced
say _ x = x

-- new construct:
sing :: String -> DancesDanced -> DancesDanced
sing _ x = x

shallowExampleProgram :: DancesDanced
shallowExampleProgram = teleport 10 10
                    $ say "hello"
                    $ dance -- 1 + 0
                    $ stop -- 0

shallowExampleProgram' = dance $ dance $ stop

-- other sem:

type TalkLog = [String]

stopTL :: TalkLog
stopTL = []

danceTL :: TalkLog -> TalkLog
danceTL rl = rl

teleportTL :: Int -> Int -> TalkLog -> TalkLog
teleportTL _ _ x = x

sayTL :: String -> TalkLog -> TalkLog
sayTL s x = s : x

-- new construct:
singTL :: String -> TalkLog -> TalkLog
singTL s x = s : x


-- what if we want to add a new language construct?
    -- easy to do in shallow embedding
    -- more involved for deep ... consider bigger examples, more complex semantics, MORE semantics ahhhh
-- what if we want a new semantics
    -- easy to in deep embedding
    -- not at all nice to do in shallow