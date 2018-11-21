{-# LANGUAGE TemplateHaskellQuotes #-}
module Obelisk.Postgres.QQ
  ( queryQ
  , executeQ
  , sqlQ
  , traceQueryQ
  ) where

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (toField, Action)
import Language.Haskell.TH (Exp, Name, Q, appE, mkName, tupE, varE, listE, sigE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))

-- | This quasiquoter is the obvious combination of 'sqlQ' and 'query'.
queryQ :: QuasiQuoter
queryQ = QuasiQuoter
  { quotePat  = error "queryQ: quasiquoter used in pattern context"
  , quoteType = error "queryQ: quasiquoter used in type context"
  , quoteExp  = \s -> appE [| uncurry query |] (sqlQExp s)
  , quoteDec  = error "queryQ: quasiquoter used in declaration context"
  }

-- | This quasiquoter is the obvious combination of 'sqlQ' and 'execute'.
executeQ :: QuasiQuoter
executeQ = QuasiQuoter
  { quotePat  = error "executeQ: quasiquoter used in pattern context"
  , quoteType = error "executeQ: quasiquoter used in type context"
  , quoteExp  = \s -> appE [| uncurry execute |] (sqlQExp s)
  , quoteDec  = error "executeQ: quasiquoter used in declaration context"
  }

traceQueryQ :: QuasiQuoter
traceQueryQ = QuasiQuoter
  { quotePat  = error "traceQueryQ: quasiquoter used in pattern context"
  , quoteType = error "traceQueryQ: quasiquoter used in type context"
  , quoteExp  = \s -> appE [| uncurry traceQuery |] (sqlQExp s)
  , quoteDec  = error "traceQueryQ: quasiquoter used in declaration context"
  }
-- | This quasiquoter takes a SQL query with named arguments in the form "?var" and generates a pair
-- consisting of the Query string itself and a tuple of variables in corresponding order.
--
-- For example: uncurry query [sqlQ| SELECT * FROM 'Book' b WHERE b.title = ?title AND b.author = ?author |]
--
-- will be equivalent to query [sql| SELECT * FROM 'Book' b WHERE b.title = ? AND b.author = ? |] [toField title, toField author]
sqlQ :: QuasiQuoter
sqlQ = QuasiQuoter
  { quotePat  = error "sqlQ: quasiquoter used in pattern context"
  , quoteType = error "sqlQ: quasiquoter used in type context"
  , quoteExp  = sqlQExp
  , quoteDec  = error "sqlQ: quasiquoter used in declaration context"
  }

sqlQExp :: String -> Q Exp
sqlQExp s =
  let (s',vs) = extractVars s
  in tupE [quoteExp sql s', sigE (listE $ map (appE (varE 'toField) . varE) vs) [t| [Action] |]]

extractVars :: String -> (String, [Name])
extractVars = extractVars'
  where
    extractVars' [] = ([],[])
    extractVars' ('?':s') =
      let [(var,rest)] = lex s'
          (s'',vars) = extractVars' rest
      in ('?':s'', mkName var : vars)
    extractVars' s' =
      let (pre,post) = break (=='?') s'
          (s'',vars) = extractVars' post
      in (pre ++ s'', vars)
