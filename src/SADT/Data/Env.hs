module SADT.Data.Env where

import qualified Data.Map as M
import SADT.Common
import SADT.Data.Expr
import SADT.Data.Type

newtype TyEnv = TyEnv (Map Var Scheme)
  deriving (Semigroup, Monoid, Show)

lookupTyEnv :: (MonadThrow m) => Var -> TyEnv -> m Scheme
lookupTyEnv x (TyEnv env) = case M.lookup x env of
  Just sc -> return sc
  _ -> throwString $ show x ++ " is not in type env."

insertTyEnv :: Var -> Scheme -> TyEnv -> TyEnv
insertTyEnv x sc (TyEnv env) = TyEnv $ M.insert x sc env

insertsTyEnv :: [(Var, Scheme)] -> TyEnv -> TyEnv
insertsTyEnv binds (TyEnv env) = TyEnv $ M.union (M.fromList $ reverse binds) env

data ConstructorInfo = CInfo
  { constructorType :: Scheme
  , patternType :: Tag
  }
  deriving (Show)

newtype ConstructorEnv = ConstructorEnv (Map Tag ConstructorInfo)
  deriving (Semigroup, Monoid, Show)

lookupCInfo :: (MonadThrow m) => Tag -> ConstructorEnv -> m ConstructorInfo
lookupCInfo tag (ConstructorEnv env) = case M.lookup tag env of
  Just info -> return info
  _ -> throwString $ show tag ++ " is not in constructor env."

insertCEnv :: Tag -> Scheme -> Tag -> ConstructorEnv -> ConstructorEnv
insertCEnv t sc pt (ConstructorEnv env) =
  ConstructorEnv $ M.insert t info env
 where
  info =
    CInfo
      { constructorType = sc
      , patternType = pt
      }
