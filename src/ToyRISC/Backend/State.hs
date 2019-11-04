-----------------------------------------------------------------------------
-- |
-- Module     : ToyRISC.Backend.State
-- Copyright  : (c) Georgy Lukyanov 2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : mail@gmail.com
-- Stability  : experimental
--
-- Pure State monad simulator backend for ToyRISC
--
-----------------------------------------------------------------------------

module ToyRISC.Backend.State
    () where




type RegisterBank a = Map.Map Register a

type Memory a = Map.Map Address a

-- | State of the execution : registers and memory
--   parameterised by the type of datapath values
data ISAState valTy = ISAState
              { _registers :: RegisterBank valTy
              -- ^ register bank
              , _memory    :: Memory valTy
              -- ^ memory
              , _pc        :: valTy
              -- ^ program counter
              } deriving Show

makeLenses ''ISAState

-----------------------------------------------------------------------------

setPC :: Imm Int -> State (ISAState SExpr) ()
setPC (Imm imm) = modify (\s -> s {_pc = constBounded imm})

readRegister :: Register -> State (ISAState a) a
readRegister reg =
  get <&> \s -> case Map.lookup reg (_registers s) of
                     Nothing -> error "readRegister: Invalid register"
                     Just v  -> v

writeRegister :: Register -> a -> State (ISAState a) ()
writeRegister reg val = modify $ \s ->
  s {_registers = Map.adjust (const val) reg (_registers s)}

readMemory :: Address -> State (ISAState a) a
readMemory reg =
  get <&> \s -> case Map.lookup reg (_memory s) of
                     Nothing -> error "readMemory: Invalid address"
                     Just v  -> v

writeMemory :: Address -> a -> State (ISAState a) ()
writeMemory addr val = modify $ \s ->
  s {_memory = Map.adjust (const val) addr (_memory s)}
