module LetLifting.LetLift where

import qualified CamOpt as C

letlift :: C.Exp -> C.Exp
letlift e = C.Letrec (reverse bindings) e'
  where
    (e', bindings) = letlift' e []

-- letlift' :: C.Exp -> [(C.Pat, C.Exp)] -> (C.Exp, [(C.Pat, C.Exp)])
-- letlift' (C.Let pat e1 e2) bindings = letlift' e2 (((pat, eNew):bs) ++ bindings)
--   where
--     (eNew, bs) = letlift' e1 []
-- letlift' (C.Letrec patexps e) bindings = letlift' e (bs ++ bindings)
--   where
--     bs = concatMap (\(p,e) -> let (eNew, bds) = letlift' e []
--                               in (p,eNew) : bds
--                    ) patexps
-- letlift' e@(C.Var _) bindings = (e, bindings)
-- letlift' (C.Sys sys) bindings = (C.Sys sysNew, bds ++ bindings)
--   where
--     (sysNew, bds) = letliftsys sys []
-- letlift' C.Void bindings = (C.Void, bindings)
-- letlift' (C.Pair e1 e2) bindings = (C.Pair e1' e2', bds2 ++ bds1 ++ bindings)
--   where
--     (e1', bds1) = letlift' e1 []
--     (e2', bds2) = letlift' e2 []
-- letlift' (C.Con tag e1) bindings = (C.Con tag e1', bds ++ bindings)
--   where
--     (e1', bds) = letlift' e1 []
-- letlift' (C.App e1 e2) bindings = (C.App e1' e2', bds2 ++ bds1 ++ bindings)
--   where
--     (e1', bds1) = letlift' e1 []
--     (e2', bds2) = letlift' e2 []
-- letlift' (C.Lam p e) bindings = (C.Lam p e', bds ++ bindings)
--   where
--     (e', bds) = letlift' e []
-- letlift' (C.If e1 e2 e3) bindings =
--   (C.If e1' e2' e3', bds3 ++ bds2 ++ bds1 ++ bindings)
--   where
--     (e1', bds1) = letlift' e1 []
--     (e2', bds2) = letlift' e2 []
--     (e3', bds3) = letlift' e3 []
-- letlift' (C.Case e patexps) bindings =
--   (C.Case e' patexps', bds' ++ bds ++ bindings)
--   where
--     (e', bds) = letlift' e []
--     temp = map (\(tf,e) -> let (e', bds') = letlift' e []
--                             in ((tf, e'), bds')) patexps
--     patexps' = map fst temp
--     bds' = concatMap snd temp
-- letlift' (C.Sequence e1 e2) bindings =
--   (C.Sequence e1' e2', bds2 ++ bds1 ++ bindings)
--   where
--     (e1', bds1) = letlift' e1 []
--     (e2', bds2) = letlift' e2 []

-- letliftsys :: C.Sys -> [(C.Pat, C.Exp)] -> (C.Sys, [(C.Pat, C.Exp)])
-- letliftsys (C.Sys2 binop e1 e2) bindings =
--   (C.Sys2 binop e1' e2', bds2 ++ bds1 ++ bindings)
--   where
--     (e1', bds1) = letlift' e1 []
--     (e2', bds2) = letlift' e2 []
-- letliftsys (C.Sys1 unaryop e1) bindings =
--   (C.Sys1 unaryop e1', bds1 ++ bindings)
--   where
--     (e1', bds1) = letlift' e1 []
-- letliftsys (C.LInt i32) bindings =
--   (C.LInt i32, bindings)
-- letliftsys (C.LFloat f) bindings =
--   (C.LFloat f, bindings)
-- letliftsys (C.LBool b) bindings =
--   (C.LBool b, bindings)
-- letliftsys (C.RTS3 rts3 e1 e2 e3) bindings=
--   (C.RTS3 rts3 e1' e2' e3', b1 ++ b2 ++ b3 ++ bindings)
--   where
--     (e1', b1) = letlift' e1 []
--     (e2', b2) = letlift' e2 []
--     (e3', b3) = letlift' e3 []
-- letliftsys (C.RTS2 rts2 e1 e2) bindings=
--   (C.RTS2 rts2 e1' e2', b1 ++ b2 ++ bindings)
--   where
--     (e1', b1) = letlift' e1 []
--     (e2', b2) = letlift' e2 []
-- letliftsys (C.RTS1 rts1 e) bindings=
--   (C.RTS1 rts1 e', b1 ++ bindings)
--   where
--     (e', b1) = letlift' e []





letlift' :: C.Exp -> [(C.Pat, C.Exp)] -> (C.Exp, [(C.Pat, C.Exp)])
letlift' (C.Let pat e1 e2) bindings = letlift' e2 (((pat, eNew):bs) ++ bindings)
  where
    (eNew, bs) = letlift' e1 []
letlift' (C.Letrec patexps e) bindings = letlift' e (bs ++ bindings)
  where
    bs = concatMap (\(p,e) -> let (eNew, bds) = letlift' e []
                              in (p,eNew) : bds
                   ) patexps
letlift' e@(C.Var _) bindings = (e, bindings)
letlift' (C.Sys sys) bindings = (C.Sys sysNew, bds ++ bindings)
  where
    (sysNew, bds) = letliftsys sys []
letlift' C.Void bindings = (C.Void, bindings)
letlift' (C.Pair e1 e2) bindings = (C.Pair e1' e2', bds2 ++ bds1 ++ bindings)
  where
    (e1', bds1) = letlift' e1 []
    (e2', bds2) = letlift' e2 []
letlift' (C.Con tag e1) bindings = (C.Con tag e1', bds ++ bindings)
  where
    (e1', bds) = letlift' e1 []
letlift' (C.App e1 e2) bindings = (C.App e1' e2', bds2 ++ bds1 ++ bindings)
  where
    (e1', bds1) = letlift' e1 []
    (e2', bds2) = letlift' e2 []
letlift' (C.Lam p e) bindings = (C.Lam p e', bds ++ bindings)
  where
    (e', bds) = letlift' e []
letlift' (C.If e1 e2 e3) bindings =
  (C.If e1' e2' e3', bds3 ++ bds2 ++ bds1 ++ bindings)
  where
    (e1', bds1) = letlift' e1 []
    (e2', bds2) = letlift' e2 []
    (e3', bds3) = letlift' e3 []
letlift' (C.Case e patexps) bindings =
  (C.Case e' patexps', bds' ++ bds ++ bindings)
  where
    (e', bds) = letlift' e []
    temp = map (\(tf,e) -> let (e', bds') = letlift' e []
                            in ((tf, e'), bds')) patexps
    patexps' = map fst temp
    bds' = concatMap snd temp
letlift' (C.Sequence e1 e2) bindings =
  (C.Sequence e1' e2', bds2 ++ bds1 ++ bindings)
  where
    (e1', bds1) = letlift' e1 []
    (e2', bds2) = letlift' e2 []

letliftsys :: C.Sys -> [(C.Pat, C.Exp)] -> (C.Sys, [(C.Pat, C.Exp)])
letliftsys (C.Sys2 binop e1 e2) bindings =
  (C.Sys2 binop e1' e2', bds2 ++ bds1 ++ bindings)
  where
    (e1', bds1) = letlift' e1 []
    (e2', bds2) = letlift' e2 []
letliftsys (C.Sys1 unaryop e1) bindings =
  (C.Sys1 unaryop e1', bds1 ++ bindings)
  where
    (e1', bds1) = letlift' e1 []
letliftsys (C.LInt i32) bindings =
  (C.LInt i32, bindings)
letliftsys (C.LFloat f) bindings =
  (C.LFloat f, bindings)
letliftsys (C.LBool b) bindings =
  (C.LBool b, bindings)
letliftsys (C.RTS3 rts3 e1 e2 e3) bindings=
  (C.RTS3 rts3 e1' e2' e3', b1 ++ b2 ++ b3 ++ bindings)
  where
    (e1', b1) = letlift' e1 []
    (e2', b2) = letlift' e2 []
    (e3', b3) = letlift' e3 []
letliftsys (C.RTS2 rts2 e1 e2) bindings=
  (C.RTS2 rts2 e1' e2', b1 ++ b2 ++ bindings)
  where
    (e1', b1) = letlift' e1 []
    (e2', b2) = letlift' e2 []
letliftsys (C.RTS1 rts1 e) bindings=
  (C.RTS1 rts1 e', b1 ++ bindings)
  where
    (e', b1) = letlift' e []
