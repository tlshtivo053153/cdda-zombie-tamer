module Define.MonsterGroup where

import Define.Core

data MGMonster = MGMonster Id Int

data MonsterGroup = MonsterGroup Id [MGMonster]
