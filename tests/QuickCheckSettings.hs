module QuickCheckSettings where

import Settings (Setting(..))

Setting1, Setting2, Setting3, Setting4, Setting5, Setting6, Setting7
Setting8, Setting9, Setting10, Setting11, Setting12, Setting13,
Setting14, Setting15, Setting16 :: Settings

Setting1 =  Settings
                ComLinear
                VarDNA
                OutLength
                (10, Just 100)
Setting2 =  Settings
                ComQuadratic 3 5
                VarPlain
                OutLength
                (3, Nothing)
Setting3 =  Settings
                ComQuadratic 10, 0
                VarWord
                OutLength
                (0, Just 5)
Setting4 =  Settings
                ComQuadratic 0 1
                VarWord
                OutLength
                (3, Just 100)
Setting5 =  Settings
                ComQuadratic 3 5
                VarDNA
                OutLength
                (0, Just 5)
Setting6 =  Settings
                ComQuadratic 10 5
                VarText
                OutLength
                (10, Just 100)
Setting7 =  Settings
                ComQuadratic 3 1
                VarPlain
                OutLength
                (0, Just 100)
Setting8 =  Settings
                ComQuadratic 0 1
                VarText
                OutLength
                (0, Nothing)
Setting9 =  Settings
                ComQuadratic 0 1
                VarPlain
                OutLength
                (0, Just 5)
Setting10 = Settings
                ComQuadratic 10 1
                VarDNA
                OutLength
                (3, Nothing)
Setting11 = Settings
                ComLinear
                VarPlain
                OutLength
                (10, Nothing)
Setting12 = Settings
                ComQuadratic 3 0
                VarText
                OutLength
                (3, Just 5)
Setting13 = Settings
                ComLinear
                VarWord
                OutLength
                (3, Just 5)
Setting14 = Settings
                ComQuadratic 3 5
                VarWord
                OutLength
                (10, Nothing)
Setting15 = Settings
                ComLinear
                VarText
                OutLength
                (0, Just 100)
Setting16 = Settings
                ComQuadratic 0 5
                VarText
                OutLength
                (0, Just 5)
Setting17 = Settings
                ComQuadratic 10 5
                VarPlain
                OutLength
                (10, Nothing)