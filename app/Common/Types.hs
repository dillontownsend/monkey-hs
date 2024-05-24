module Common.Types where

import Common.Trans.State

type Input = String

type LexerState a = State Input a
