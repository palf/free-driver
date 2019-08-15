{-# LANGUAGE DeriveFunctor #-}

module Drive.Terminal.Types
  ( TerminalF (..)
  ) where


data TerminalF a
  = WaitForEnter a
  | PrintMessage String a
  | ReadInput (String -> a)
  deriving (Functor)
