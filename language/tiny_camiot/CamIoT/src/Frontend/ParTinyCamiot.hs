{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParTinyCamiot where
import qualified AbsTinyCamiot
import LexTinyCamiot
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap40 = HappyWrap40 (AbsTinyCamiot.Ident)
happyIn40 :: (AbsTinyCamiot.Ident) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 (Integer)
happyIn41 :: (Integer) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 (Double)
happyIn42 :: (Double) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 (AbsTinyCamiot.UIdent)
happyIn43 :: (AbsTinyCamiot.UIdent) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 ([AbsTinyCamiot.Def ()])
happyIn44 :: ([AbsTinyCamiot.Def ()]) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 ((AbsTinyCamiot.Def ()))
happyIn45 :: ((AbsTinyCamiot.Def ())) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 ((AbsTinyCamiot.ConstructorDec ()))
happyIn46 :: ((AbsTinyCamiot.ConstructorDec ())) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 ([AbsTinyCamiot.ConstructorDec ()])
happyIn47 :: ([AbsTinyCamiot.ConstructorDec ()]) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 ([AbsTinyCamiot.Ident])
happyIn48 :: ([AbsTinyCamiot.Ident]) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 ((AbsTinyCamiot.Type ()))
happyIn49 :: ((AbsTinyCamiot.Type ())) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 (AbsTinyCamiot.Type ())
happyIn50 :: (AbsTinyCamiot.Type ()) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 (AbsTinyCamiot.Type ())
happyIn51 :: (AbsTinyCamiot.Type ()) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 ((AbsTinyCamiot.TupType ()))
happyIn52 :: ((AbsTinyCamiot.TupType ())) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 ([AbsTinyCamiot.TupType ()])
happyIn53 :: ([AbsTinyCamiot.TupType ()]) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 ([AbsTinyCamiot.Type ()])
happyIn54 :: ([AbsTinyCamiot.Type ()]) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
newtype HappyWrap55 = HappyWrap55 ((AbsTinyCamiot.TupExp ()))
happyIn55 :: ((AbsTinyCamiot.TupExp ())) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap55 x)
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> HappyWrap55
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
newtype HappyWrap56 = HappyWrap56 ([AbsTinyCamiot.TupExp ()])
happyIn56 :: ([AbsTinyCamiot.TupExp ()]) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap56 x)
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> HappyWrap56
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
newtype HappyWrap57 = HappyWrap57 ((AbsTinyCamiot.Exp ()))
happyIn57 :: ((AbsTinyCamiot.Exp ())) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap57 x)
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> HappyWrap57
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
newtype HappyWrap58 = HappyWrap58 (AbsTinyCamiot.Exp ())
happyIn58 :: (AbsTinyCamiot.Exp ()) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap58 x)
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> HappyWrap58
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
newtype HappyWrap59 = HappyWrap59 (AbsTinyCamiot.Exp ())
happyIn59 :: (AbsTinyCamiot.Exp ()) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap59 x)
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> HappyWrap59
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
newtype HappyWrap60 = HappyWrap60 (AbsTinyCamiot.Exp ())
happyIn60 :: (AbsTinyCamiot.Exp ()) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap60 x)
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> HappyWrap60
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
newtype HappyWrap61 = HappyWrap61 (AbsTinyCamiot.Exp ())
happyIn61 :: (AbsTinyCamiot.Exp ()) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap61 x)
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> HappyWrap61
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
newtype HappyWrap62 = HappyWrap62 (AbsTinyCamiot.Exp ())
happyIn62 :: (AbsTinyCamiot.Exp ()) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap62 x)
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> HappyWrap62
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
newtype HappyWrap63 = HappyWrap63 (AbsTinyCamiot.Exp ())
happyIn63 :: (AbsTinyCamiot.Exp ()) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap63 x)
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> HappyWrap63
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
newtype HappyWrap64 = HappyWrap64 (AbsTinyCamiot.Exp ())
happyIn64 :: (AbsTinyCamiot.Exp ()) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap64 x)
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> HappyWrap64
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
newtype HappyWrap65 = HappyWrap65 ((AbsTinyCamiot.AddOp ()))
happyIn65 :: ((AbsTinyCamiot.AddOp ())) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap65 x)
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> HappyWrap65
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
newtype HappyWrap66 = HappyWrap66 ((AbsTinyCamiot.MulOp ()))
happyIn66 :: ((AbsTinyCamiot.MulOp ())) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap66 x)
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> HappyWrap66
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
newtype HappyWrap67 = HappyWrap67 ((AbsTinyCamiot.RelOp ()))
happyIn67 :: ((AbsTinyCamiot.RelOp ())) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap67 x)
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> HappyWrap67
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
newtype HappyWrap68 = HappyWrap68 ([AbsTinyCamiot.Exp ()])
happyIn68 :: ([AbsTinyCamiot.Exp ()]) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap68 x)
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> HappyWrap68
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
newtype HappyWrap69 = HappyWrap69 ((AbsTinyCamiot.Con ()))
happyIn69 :: ((AbsTinyCamiot.Con ())) -> (HappyAbsSyn )
happyIn69 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap69 x)
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> HappyWrap69
happyOut69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut69 #-}
newtype HappyWrap70 = HappyWrap70 ((AbsTinyCamiot.Const ()))
happyIn70 :: ((AbsTinyCamiot.Const ())) -> (HappyAbsSyn )
happyIn70 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap70 x)
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> HappyWrap70
happyOut70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut70 #-}
newtype HappyWrap71 = HappyWrap71 ((AbsTinyCamiot.Pat ()))
happyIn71 :: ((AbsTinyCamiot.Pat ())) -> (HappyAbsSyn )
happyIn71 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap71 x)
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> HappyWrap71
happyOut71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut71 #-}
newtype HappyWrap72 = HappyWrap72 (AbsTinyCamiot.Pat ())
happyIn72 :: (AbsTinyCamiot.Pat ()) -> (HappyAbsSyn )
happyIn72 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap72 x)
{-# INLINE happyIn72 #-}
happyOut72 :: (HappyAbsSyn ) -> HappyWrap72
happyOut72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut72 #-}
newtype HappyWrap73 = HappyWrap73 (AbsTinyCamiot.Pat ())
happyIn73 :: (AbsTinyCamiot.Pat ()) -> (HappyAbsSyn )
happyIn73 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap73 x)
{-# INLINE happyIn73 #-}
happyOut73 :: (HappyAbsSyn ) -> HappyWrap73
happyOut73 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut73 #-}
newtype HappyWrap74 = HappyWrap74 ((AbsTinyCamiot.AdtPat ()))
happyIn74 :: ((AbsTinyCamiot.AdtPat ())) -> (HappyAbsSyn )
happyIn74 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap74 x)
{-# INLINE happyIn74 #-}
happyOut74 :: (HappyAbsSyn ) -> HappyWrap74
happyOut74 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut74 #-}
newtype HappyWrap75 = HappyWrap75 ([AbsTinyCamiot.AdtPat ()])
happyIn75 :: ([AbsTinyCamiot.AdtPat ()]) -> (HappyAbsSyn )
happyIn75 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap75 x)
{-# INLINE happyIn75 #-}
happyOut75 :: (HappyAbsSyn ) -> HappyWrap75
happyOut75 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut75 #-}
newtype HappyWrap76 = HappyWrap76 ((AbsTinyCamiot.TupPat ()))
happyIn76 :: ((AbsTinyCamiot.TupPat ())) -> (HappyAbsSyn )
happyIn76 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap76 x)
{-# INLINE happyIn76 #-}
happyOut76 :: (HappyAbsSyn ) -> HappyWrap76
happyOut76 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut76 #-}
newtype HappyWrap77 = HappyWrap77 ([AbsTinyCamiot.TupPat ()])
happyIn77 :: ([AbsTinyCamiot.TupPat ()]) -> (HappyAbsSyn )
happyIn77 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap77 x)
{-# INLINE happyIn77 #-}
happyOut77 :: (HappyAbsSyn ) -> HappyWrap77
happyOut77 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut77 #-}
newtype HappyWrap78 = HappyWrap78 ([AbsTinyCamiot.Pat ()])
happyIn78 :: ([AbsTinyCamiot.Pat ()]) -> (HappyAbsSyn )
happyIn78 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap78 x)
{-# INLINE happyIn78 #-}
happyOut78 :: (HappyAbsSyn ) -> HappyWrap78
happyOut78 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut78 #-}
newtype HappyWrap79 = HappyWrap79 ((AbsTinyCamiot.PatMatch ()))
happyIn79 :: ((AbsTinyCamiot.PatMatch ())) -> (HappyAbsSyn )
happyIn79 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap79 x)
{-# INLINE happyIn79 #-}
happyOut79 :: (HappyAbsSyn ) -> HappyWrap79
happyOut79 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut79 #-}
newtype HappyWrap80 = HappyWrap80 ([AbsTinyCamiot.PatMatch ()])
happyIn80 :: ([AbsTinyCamiot.PatMatch ()]) -> (HappyAbsSyn )
happyIn80 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap80 x)
{-# INLINE happyIn80 #-}
happyOut80 :: (HappyAbsSyn ) -> HappyWrap80
happyOut80 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut80 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\xd0\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x1a\x00\x40\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x40\x03\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x68\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x0d\x00\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\xa0\x01\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x06\x00\xc8\xa4\x81\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x00\x99\x34\xf0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x20\x93\x06\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x03\x00\x24\x00\xc0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x68\x00\x80\x04\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x90\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x01\x00\x12\x00\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x40\x02\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x06\x00\x48\x00\x80\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x09\x00\xf0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x90\x49\x03\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x40\x02\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x48\x01\x80\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\xa4\x00\xc0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x80\x14\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x90\x02\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x52\x00\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x40\x0a\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x48\x01\x80\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x29\x00\xf0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x52\x00\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x40\x0a\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x52\x00\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\xa4\x00\xc0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x90\x02\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x68\x00\x80\x4c\x1a\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\xb0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x12\x00\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x40\x26\x0d\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x48\x01\x80\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x00\x99\x34\xf0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x20\x93\x06\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\xa4\x00\xc0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x80\x14\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x40\x02\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\xd0\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x68\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x01\x52\x00\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x0d\x00\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x34\x00\x80\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x1a\x00\x40\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x40\x03\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x00\x99\x34\xf0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x03\x00\x24\x00\xc0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x68\x00\x80\x04\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x90\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x01\x00\x12\x00\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x40\x02\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x80\x14\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x52\x00\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x48\x01\x80\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x20\x05\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x03\x00\x64\xd2\xc0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x40\x26\x0d\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x00\x99\x34\xf0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x20\x93\x06\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x03\x00\x64\xd2\xc0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x40\x26\x0d\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x40\x0a\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x03\x00\x64\xd2\xc0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x68\x00\x80\x4c\x1a\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x90\x49\x03\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pListDef","%start_pDef","%start_pConstructorDec","%start_pListConstructorDec","%start_pListIdent","%start_pType","%start_pType1","%start_pType2","%start_pTupType","%start_pListTupType","%start_pListType1","%start_pTupExp","%start_pListTupExp","%start_pExp","%start_pExp6","%start_pExp1","%start_pExp2","%start_pExp3","%start_pExp4","%start_pExp5","%start_pExp7","%start_pAddOp","%start_pMulOp","%start_pRelOp","%start_pListExp","%start_pCon","%start_pConst","%start_pPat","%start_pPat1","%start_pPat2","%start_pAdtPat","%start_pListAdtPat","%start_pTupPat","%start_pListTupPat","%start_pListPat","%start_pPatMatch","%start_pListPatMatch","Ident","Integer","Double","UIdent","ListDef","Def","ConstructorDec","ListConstructorDec","ListIdent","Type","Type1","Type2","TupType","ListTupType","ListType1","TupExp","ListTupExp","Exp","Exp6","Exp1","Exp2","Exp3","Exp4","Exp5","Exp7","AddOp","MulOp","RelOp","ListExp","Con","Const","Pat","Pat1","Pat2","AdtPat","ListAdtPat","TupPat","ListTupPat","ListPat","PatMatch","ListPatMatch","'!'","'&&'","'('","'()'","')'","'*'","'+'","','","'-'","'->'","'/'","':'","';'","'<'","'<='","'='","'=='","'>'","'>='","'Bool'","'False'","'Float'","'Int'","'True'","'\\\\'","'_'","'as'","'case'","'data'","'else'","'if'","'in'","'let'","'letrec'","'of'","'then'","'where'","'{'","'||'","'}'","L_Ident","L_integ","L_doubl","L_UIdent","%eof"]
        bit_start = st * 125
        bit_end = (st + 1) * 125
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..124]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xe9\xff\xe9\xff\xd5\xff\xd5\xff\xe5\xff\x5a\x00\x5a\x00\x11\x00\x5a\x00\x5a\x00\x5a\x00\x01\x00\x01\x00\x01\x00\x06\x00\x06\x00\x06\x00\x06\x00\x06\x00\x06\x00\x3f\x00\xf1\x00\x47\x00\xa0\x01\x01\x00\xf0\xff\x98\x00\x30\x00\x05\x00\x46\x00\x30\x00\x30\x00\x30\x00\x30\x00\x30\x00\x30\x00\x30\x00\xe8\xff\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x6e\x00\x54\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x54\x00\x30\x00\x54\x00\x00\x00\x64\x00\x55\x00\x55\x00\x00\x00\x30\x00\x55\x00\x55\x00\x74\x00\x77\x00\x30\x00\x77\x00\x30\x00\x77\x00\x77\x00\x00\x00\x77\x00\x00\x00\x00\x00\x01\x00\x3f\x00\x00\x00\x8a\x00\x6c\x03\xf1\x00\x47\x00\x00\x00\x8e\x00\x00\x00\x3f\x00\x01\x00\x30\x00\x01\x00\x01\x00\x30\x00\x30\x00\x8e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8e\x00\x00\x00\x00\x00\x8e\x00\x00\x00\x00\x00\x8e\x00\x40\x00\x4f\x00\xbc\x00\x8e\x00\x8e\x00\x0c\x00\x8e\x00\xc4\x00\x94\x00\x00\x00\x94\x00\x00\x00\x5a\x00\x5a\x00\x00\x00\x94\x00\x5a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\x00\xc8\x00\xaf\x00\xaf\x00\xaf\x00\x5a\x00\xaf\x00\xaf\x00\xd0\x00\xce\x00\xf5\x00\xf3\x00\xdc\x00\xdc\x00\x3b\x00\xdc\x00\xea\x00\xe2\x00\x0a\x01\xe9\xff\xf6\x00\x0e\x01\x5a\x00\xfa\x00\x5a\x00\x00\x00\x1b\x01\x5a\x00\x5a\x00\x1b\x01\x1c\x01\x00\x00\x00\x00\x01\x00\x00\x00\x06\x00\x06\x00\x06\x00\x17\x01\x1d\x01\x10\x01\x0b\x01\x2b\x01\x3d\x01\x56\x01\x00\x00\x06\x00\x06\x00\x00\x00\x74\x01\x75\x01\x90\x01\x30\x00\x00\x00\x30\x00\x00\x00\x30\x00\x00\x00\x30\x00\x01\x00\x00\x00\x00\x00\x93\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x6e\x01\x01\x00\x01\x00\x01\x00\x3f\x00\x47\x00\xf1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x77\x01\x00\x00\x86\x01\x00\x00\x94\x01\x95\x01\x92\x01\x30\x00\x00\x00\x00\x00\x8e\x01\x01\x00\x01\x00\x01\x00\xd6\x01\xe4\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x13\x00\xe3\x00\x3e\x00\x30\x01\x03\x00\xcb\x02\x7a\x00\xb9\x00\xe4\x02\x95\x03\x80\x03\xda\x01\x8f\x01\x31\x02\xfe\x01\x51\x03\xcc\x01\x3c\x02\x55\x02\x87\x02\xf0\x00\xf4\x01\xfd\x01\xff\x01\xf3\x01\x7b\x00\x0b\x00\x43\x01\x5f\x00\x1f\x00\x3e\x01\x1a\x01\x04\x01\xc6\x00\x9f\x00\x8b\x00\x72\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x01\x00\x00\x00\x00\x00\x00\x00\x00\x53\x01\x00\x00\xec\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x02\x7b\x01\x00\x00\x00\x00\x00\x02\xf5\x01\x07\x02\x00\x00\x00\x00\x00\x00\x7f\x01\xa8\x01\x57\x01\x4a\x02\x63\x02\x65\x01\x69\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x02\x09\x02\x10\x02\x00\x00\x00\x00\xb9\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x03\x8f\x03\x00\x00\x00\x00\xa3\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfd\x02\x00\x00\x00\x00\x9b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb7\x00\x00\x00\x15\x02\x00\x00\x00\x00\x96\x01\xda\x00\x00\x00\x16\x03\xc2\x01\x2f\x03\x00\x00\x00\x00\xa8\x03\xb6\x03\x00\x00\x00\x00\x00\x00\x00\x00\xc1\x01\x00\x00\x6e\x02\xa0\x02\x1d\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe5\x01\x5e\x03\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x01\x00\x00\xf4\x00\x00\x00\x2f\x01\x00\x00\x76\x00\x7c\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x95\x02\x00\x00\xae\x02\xc7\x02\xe0\x02\xb9\x02\x13\x02\x1e\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf9\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x87\x00\x00\x00\x00\x00\x00\x00\x12\x03\x2b\x03\x44\x03\xc4\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xd6\xff\x00\x00\x00\x00\xcf\xff\xcc\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbe\xff\xbb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x95\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\xff\x7b\xff\x00\x00\x00\x00\x00\x00\xda\xff\x8c\xff\x92\xff\x91\xff\x8b\xff\x8d\xff\x00\x00\x87\xff\x85\xff\x78\xff\x00\x00\x00\x00\x8e\xff\x8f\xff\x90\xff\x89\xff\xd9\xff\xd8\xff\xd7\xff\x00\x00\x7b\xff\x00\x00\x7f\xff\x7d\xff\x00\x00\x00\x00\x82\xff\x81\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\xff\x00\x00\x00\x00\x93\xff\x00\x00\xa2\xff\xa1\xff\x95\xff\xa4\xff\xb1\xff\xac\xff\xaa\xff\xa8\xff\xa6\xff\xae\xff\x00\x00\xa0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9a\xff\x99\xff\x96\xff\x98\xff\x97\xff\x00\x00\x9c\xff\x9b\xff\x00\x00\x9e\xff\x9d\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb8\xff\x00\x00\xb9\xff\x00\x00\xc8\xff\xbb\xff\xbb\xff\xc6\xff\x00\x00\xbe\xff\xc5\xff\xc3\xff\xc1\xff\xc2\xff\xbf\xff\xc9\xff\xbd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcc\xff\x00\x00\x00\x00\xce\xff\x00\x00\x00\x00\x7b\xff\x00\x00\x00\x00\x00\x00\xd5\xff\xd6\xff\xcc\xff\x00\x00\x00\x00\xcf\xff\x00\x00\xcb\xff\x00\x00\xbe\xff\x00\x00\xbf\xff\x00\x00\xba\xff\xc4\xff\x00\x00\xb0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb9\xff\xaf\xff\x00\x00\x00\x00\x94\xff\x7f\xff\x00\x00\x00\x00\x00\x00\x80\xff\x7e\xff\x7a\xff\x8b\xff\x88\xff\x00\x00\x00\x00\x79\xff\x77\xff\x00\x00\x7c\xff\x84\xff\x83\xff\x86\xff\xad\xff\xab\xff\x9f\xff\xa3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa5\xff\xa7\xff\xa9\xff\xb7\xff\xc7\xff\xc0\xff\xca\xff\xbc\xff\xd0\xff\xcd\xff\xd2\xff\x00\x00\x00\x00\xd4\xff\x00\x00\xd3\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb3\xff\x8a\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcf\xff\x00\x00\xb4\xff\xb5\xff\xb2\xff\xb6\xff\xd1\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x2c\x00\x01\x00\x00\x00\x03\x00\x04\x00\x1d\x00\x01\x00\x03\x00\x03\x00\x04\x00\x08\x00\x01\x00\x02\x00\x29\x00\x03\x00\x04\x00\x29\x00\x29\x00\x00\x00\x03\x00\x04\x00\x15\x00\x04\x00\x05\x00\x18\x00\x19\x00\x15\x00\x2c\x00\x1c\x00\x18\x00\x00\x00\x1f\x00\x15\x00\x21\x00\x22\x00\x18\x00\x14\x00\x0a\x00\x16\x00\x17\x00\x1e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x29\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x03\x00\x04\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x03\x00\x04\x00\x05\x00\x2c\x00\x03\x00\x04\x00\x21\x00\x03\x00\x03\x00\x04\x00\x06\x00\x15\x00\x06\x00\x0c\x00\x18\x00\x03\x00\x1a\x00\x0b\x00\x15\x00\x06\x00\x1b\x00\x18\x00\x15\x00\x1a\x00\x0b\x00\x18\x00\x15\x00\x1a\x00\x07\x00\x18\x00\x09\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x03\x00\x04\x00\x00\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x08\x00\x2d\x00\x14\x00\x29\x00\x16\x00\x17\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x0d\x00\x2d\x00\x03\x00\x03\x00\x20\x00\x21\x00\x2d\x00\x2d\x00\x29\x00\x0a\x00\x0b\x00\x2c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x1d\x00\x27\x00\x28\x00\x00\x00\x04\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x08\x00\x2d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x15\x00\x27\x00\x28\x00\x18\x00\x27\x00\x27\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x2d\x00\x03\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x2d\x00\x2a\x00\x2b\x00\x0b\x00\x26\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0e\x00\x0f\x00\x08\x00\x11\x00\x12\x00\x13\x00\x08\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x26\x00\x00\x00\x0a\x00\x2d\x00\x26\x00\x00\x00\x01\x00\x02\x00\x03\x00\x08\x00\x00\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x05\x00\x2d\x00\x24\x00\x25\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x29\x00\x09\x00\x2d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x0d\x00\x0c\x00\x24\x00\x25\x00\x00\x00\x01\x00\x02\x00\x03\x00\x18\x00\x2d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x1e\x00\x2d\x00\x24\x00\x25\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x2c\x00\x0d\x00\x24\x00\x25\x00\x00\x00\x01\x00\x02\x00\x03\x00\x10\x00\x29\x00\x05\x00\x05\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x2c\x00\x10\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x10\x00\x23\x00\x00\x00\x01\x00\x02\x00\x03\x00\x03\x00\x24\x00\x0a\x00\x06\x00\x07\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x01\x00\x02\x00\x03\x00\x05\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x05\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x05\x00\x05\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x00\x00\x01\x00\x02\x00\x03\x00\x18\x00\x26\x00\x05\x00\x00\x00\x18\x00\x05\x00\x1e\x00\x04\x00\x05\x00\x25\x00\x1e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x26\x00\x1e\x00\x0e\x00\x0f\x00\x1e\x00\x11\x00\x12\x00\x13\x00\x20\x00\x20\x00\x28\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x03\x00\x1e\x00\x03\x00\x06\x00\x07\x00\x06\x00\x07\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x12\x00\x1e\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0f\x00\x1e\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x12\x00\x1e\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x2c\x00\x1e\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x28\x00\x19\x00\x19\x00\x1c\x00\x12\x00\x1e\x00\x00\x00\x01\x00\x02\x00\x03\x00\x18\x00\x1a\x00\x03\x00\xff\xff\x1b\x00\x1b\x00\x1e\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1a\x00\x19\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x1b\x00\xff\xff\x1a\x00\x1c\x00\x12\x00\x1e\x00\x00\x00\x01\x00\x02\x00\x03\x00\x18\x00\xff\xff\x19\x00\xff\xff\xff\xff\xff\xff\x1e\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x12\x00\x1e\x00\xff\xff\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x1e\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x12\x00\x1e\x00\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x1e\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x12\x00\x1e\x00\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x1e\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x12\x00\x1e\x00\xff\xff\xff\xff\xff\xff\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x1e\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x12\x00\x1e\x00\xff\xff\xff\xff\xff\xff\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x1e\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x1e\x00\xff\xff\x03\x00\xff\xff\xff\xff\x18\x00\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x1e\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x1e\x00\xff\xff\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x1e\x00\xff\xff\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x1e\x00\xff\xff\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x1e\x00\xff\xff\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x1e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x1e\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\xff\xff\xff\xff\xff\xff\x02\x00\x1e\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\xff\xff\xff\xff\x0e\x00\x0f\x00\x1e\x00\x11\x00\x12\x00\x13\x00\x00\x00\xff\xff\x00\x00\x03\x00\xff\xff\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\x0a\x00\x0b\x00\x0e\x00\x00\x00\x0e\x00\xff\xff\x03\x00\xff\xff\xff\xff\x00\x00\xff\xff\xff\xff\x03\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\xff\xff\xff\xff\x03\x00\xff\xff\x00\x00\xff\xff\xff\xff\x03\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\xff\xff\xff\xff\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x39\x00\x5a\x00\x8b\x00\x5b\x00\x33\x00\x94\x00\x5a\x00\x49\x00\x5b\x00\x33\x00\x8c\x00\x28\x00\x29\x00\x27\x00\x5b\x00\x33\x00\x27\x00\x27\x00\x91\x00\x89\x00\x7f\x00\x34\x00\x94\x00\x95\x00\x35\x00\x5c\x00\x34\x00\x39\x00\x5d\x00\x35\x00\x44\x00\x5e\x00\x34\x00\x5f\x00\x60\x00\x35\x00\x80\x00\xbf\x00\x81\x00\x82\x00\x4a\x00\x27\x00\x37\x00\x38\x00\x39\x00\x27\x00\x27\x00\x37\x00\x38\x00\x39\x00\x32\x00\x33\x00\x27\x00\x37\x00\x38\x00\x39\x00\xff\xff\x32\x00\x33\x00\xbd\x00\x39\x00\x32\x00\x33\x00\x45\x00\x8d\x00\x5b\x00\x33\x00\x90\x00\x34\x00\x68\x00\x9a\x00\x35\x00\x47\x00\x36\x00\x69\x00\x34\x00\x68\x00\xb8\x00\x35\x00\x34\x00\x36\x00\x69\x00\x35\x00\x34\x00\x36\x00\x6b\x00\x35\x00\x6c\x00\x27\x00\x37\x00\x38\x00\x39\x00\x7e\x00\x7f\x00\x44\x00\x27\x00\x37\x00\x38\x00\x39\x00\x27\x00\x37\x00\x38\x00\x39\x00\x27\x00\x37\x00\x38\x00\x39\x00\xba\x00\xff\xff\x80\x00\x27\x00\x81\x00\x82\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x78\x00\xbe\x00\xff\xff\x79\x00\x4b\x00\x47\x00\x2e\x00\xff\xff\xff\xff\x27\x00\x89\x00\x7b\x00\x39\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x27\x00\x28\x00\x29\x00\x2a\x00\xb8\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x4c\x00\x2f\x00\x30\x00\x8b\x00\x33\x00\x2f\x00\xc0\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x9c\x00\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x34\x00\x2f\x00\xe5\x00\x35\x00\xb3\x00\x39\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x27\x00\x28\x00\x29\x00\x2a\x00\xff\xff\x79\x00\x2b\x00\x3a\x00\x2d\x00\x2e\x00\xff\xff\x37\x00\x38\x00\x87\x00\x3b\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x62\x00\x63\x00\xa5\x00\x64\x00\x65\x00\x66\x00\x9f\x00\x2b\x00\x3a\x00\x2d\x00\x2e\x00\x2b\x00\x3a\x00\x2d\x00\x2e\x00\xba\x00\x8b\x00\xa0\x00\xff\xff\x98\x00\x27\x00\x28\x00\x29\x00\xbb\x00\xdb\x00\x91\x00\x2b\x00\x3c\x00\x2d\x00\x2e\x00\x92\x00\xff\xff\x3d\x00\x3e\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x6b\x00\x27\x00\x6c\x00\xff\xff\x2b\x00\xb4\x00\x2d\x00\x2e\x00\x9b\x00\x9c\x00\x3d\x00\xb5\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x6c\x00\xff\xff\x2b\x00\xb4\x00\x2d\x00\x2e\x00\x58\x00\xff\xff\x3d\x00\xb5\x00\x2b\x00\x3c\x00\x2d\x00\x2e\x00\x39\x00\x97\x00\x3d\x00\xc2\x00\x27\x00\x28\x00\x29\x00\x2a\x00\xdb\x00\x27\x00\xd5\x00\xd4\x00\x2b\x00\x3c\x00\x2d\x00\x2e\x00\x39\x00\xcf\x00\x3f\x00\x27\x00\x28\x00\x29\x00\x2a\x00\xce\x00\xcc\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x8d\x00\xcd\x00\xcb\x00\x8e\x00\x8f\x00\x2b\x00\x40\x00\x2d\x00\x2e\x00\x41\x00\x42\x00\x27\x00\x28\x00\x29\x00\x2a\x00\xca\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x40\x00\x2d\x00\x2e\x00\x41\x00\xb8\x00\x2b\x00\x40\x00\x2d\x00\x2e\x00\x41\x00\xc1\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x27\x00\x28\x00\x29\x00\x2a\x00\xc9\x00\x2b\x00\x40\x00\x2d\x00\x2e\x00\x43\x00\x2b\x00\x49\x00\x2d\x00\x2e\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\xb6\x00\x2d\x00\x2e\x00\x2b\x00\xad\x00\x2d\x00\x2e\x00\xc5\x00\xc6\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x2b\x00\xaa\x00\x2d\x00\x2e\x00\x2b\x00\xa9\x00\x2d\x00\x2e\x00\x2b\x00\xc3\x00\x2d\x00\x2e\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\xa5\x00\xe3\x00\xc5\x00\x91\x00\xb0\x00\xe5\x00\x58\x00\xdc\x00\x95\x00\xde\x00\x58\x00\x74\x00\x75\x00\x76\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\xea\x00\x58\x00\x62\x00\x63\x00\xe7\x00\x64\x00\x65\x00\x66\x00\xe9\x00\xe8\x00\xef\x00\x74\x00\xae\x00\xaf\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x8d\x00\x58\x00\x8d\x00\x8e\x00\xd8\x00\x8e\x00\xea\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x74\x00\xd2\x00\x76\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x50\x00\x58\x00\x70\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x77\x00\x58\x00\x76\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x50\x00\x58\x00\xc7\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x39\x00\x58\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\xf0\x00\x69\x00\xa7\x00\x57\x00\x72\x00\x58\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x56\x00\x66\x00\x97\x00\x00\x00\x60\x00\xa6\x00\x58\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\xa8\x00\xa7\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\xa6\x00\x00\x00\xa8\x00\xb3\x00\xcf\x00\x58\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x56\x00\x00\x00\xa7\x00\x00\x00\x00\x00\x00\x00\x58\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x00\x00\x00\x00\x73\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x50\x00\x58\x00\x00\x00\x6f\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x00\x00\x58\x00\xac\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x50\x00\x58\x00\x00\x00\x00\x00\x6e\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x00\x00\x58\x00\xab\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x50\x00\x58\x00\x00\x00\x00\x00\xd1\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x00\x00\x58\x00\xbf\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x50\x00\x58\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x00\x00\x58\x00\xe3\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x50\x00\x58\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x00\x00\x58\x00\xe1\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x78\x00\x58\x00\x00\x00\x79\x00\x00\x00\x00\x00\xa5\x00\x00\x00\x00\x00\x8a\x00\x83\x00\x7b\x00\x58\x00\xe0\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x78\x00\x58\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x83\x00\x7b\x00\x86\x00\xdf\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x78\x00\x58\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9d\x00\x83\x00\x7b\x00\x00\x00\xde\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x78\x00\x58\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd9\x00\x83\x00\x7b\x00\x00\x00\xed\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x78\x00\x58\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd7\x00\x83\x00\x7b\x00\x00\x00\xec\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x00\x00\x58\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\xeb\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x00\x00\x4d\x00\x28\x00\x29\x00\x4e\x00\x58\x00\x50\x00\x71\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x58\x00\x50\x00\xc6\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x00\x00\x00\x00\x00\x00\x62\x00\x63\x00\x58\x00\x64\x00\x65\x00\x66\x00\x78\x00\x00\x00\x78\x00\x79\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7a\x00\x7b\x00\x7a\x00\x7b\x00\x7c\x00\x78\x00\xa3\x00\x00\x00\x79\x00\x00\x00\x00\x00\x78\x00\x00\x00\x00\x00\x79\x00\x7a\x00\x7b\x00\x00\x00\x00\x00\xa2\x00\x82\x00\x83\x00\x7b\x00\x84\x00\x85\x00\x78\x00\x00\x00\x00\x00\x79\x00\x00\x00\x78\x00\x00\x00\x00\x00\x79\x00\xa0\x00\x83\x00\x7b\x00\x84\x00\xa1\x00\x82\x00\x83\x00\x7b\x00\x84\x00\xd6\x00\x78\x00\x00\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd5\x00\x83\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (37, 136) [
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136)
	]

happy_n_terms = 46 :: Int
happy_n_nonterms = 41 :: Int

happyReduce_37 = happySpecReduce_1  0# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn40
		 (AbsTinyCamiot.Ident happy_var_1
	)}

happyReduce_38 = happySpecReduce_1  1# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn41
		 ((read (happy_var_1)) :: Integer
	)}

happyReduce_39 = happySpecReduce_1  2# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TD happy_var_1)) -> 
	happyIn42
		 ((read (happy_var_1)) :: Double
	)}

happyReduce_40 = happySpecReduce_1  3# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_UIdent happy_var_1)) -> 
	happyIn43
		 (AbsTinyCamiot.UIdent happy_var_1
	)}

happyReduce_41 = happySpecReduce_0  4# happyReduction_41
happyReduction_41  =  happyIn44
		 ([]
	)

happyReduce_42 = happySpecReduce_1  4# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	happyIn44
		 ((:[]) happy_var_1
	)}

happyReduce_43 = happySpecReduce_3  4# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	case happyOut44 happy_x_3 of { (HappyWrap44 happy_var_3) -> 
	happyIn44
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_44 = happyReduce 4# 5# happyReduction_44
happyReduction_44 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	case happyOut78 happy_x_2 of { (HappyWrap78 happy_var_2) -> 
	case happyOut57 happy_x_4 of { (HappyWrap57 happy_var_4) -> 
	happyIn45
		 (AbsTinyCamiot.DEquation () happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_45 = happySpecReduce_3  5# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	case happyOut49 happy_x_3 of { (HappyWrap49 happy_var_3) -> 
	happyIn45
		 (AbsTinyCamiot.DTypeSig () happy_var_1 happy_var_3
	)}}

happyReduce_46 = happyReduce 7# 5# happyReduction_46
happyReduction_46 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	case happyOut48 happy_x_3 of { (HappyWrap48 happy_var_3) -> 
	case happyOut47 happy_x_6 of { (HappyWrap47 happy_var_6) -> 
	happyIn45
		 (AbsTinyCamiot.DDataDec () happy_var_2 happy_var_3 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_47 = happySpecReduce_3  6# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	case happyOut49 happy_x_3 of { (HappyWrap49 happy_var_3) -> 
	happyIn46
		 (AbsTinyCamiot.ConstDec () happy_var_1 happy_var_3
	)}}

happyReduce_48 = happySpecReduce_0  7# happyReduction_48
happyReduction_48  =  happyIn47
		 ([]
	)

happyReduce_49 = happySpecReduce_1  7# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	happyIn47
		 ((:[]) happy_var_1
	)}

happyReduce_50 = happySpecReduce_3  7# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	case happyOut47 happy_x_3 of { (HappyWrap47 happy_var_3) -> 
	happyIn47
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_51 = happySpecReduce_0  8# happyReduction_51
happyReduction_51  =  happyIn48
		 ([]
	)

happyReduce_52 = happySpecReduce_2  8# happyReduction_52
happyReduction_52 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	case happyOut48 happy_x_2 of { (HappyWrap48 happy_var_2) -> 
	happyIn48
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_53 = happySpecReduce_3  9# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	case happyOut49 happy_x_3 of { (HappyWrap49 happy_var_3) -> 
	happyIn49
		 (AbsTinyCamiot.TLam () happy_var_1 happy_var_3
	)}}

happyReduce_54 = happySpecReduce_1  9# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	happyIn49
		 (happy_var_1
	)}

happyReduce_55 = happySpecReduce_1  10# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn50
		 (AbsTinyCamiot.TVar () happy_var_1
	)}

happyReduce_56 = happySpecReduce_3  10# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_2 of { (HappyWrap53 happy_var_2) -> 
	happyIn50
		 (AbsTinyCamiot.TTup () happy_var_2
	)}

happyReduce_57 = happySpecReduce_1  10# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	happyIn50
		 (happy_var_1
	)}

happyReduce_58 = happySpecReduce_1  11# happyReduction_58
happyReduction_58 happy_x_1
	 =  happyIn51
		 (AbsTinyCamiot.TNil ()
	)

happyReduce_59 = happySpecReduce_2  11# happyReduction_59
happyReduction_59 happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	case happyOut54 happy_x_2 of { (HappyWrap54 happy_var_2) -> 
	happyIn51
		 (AbsTinyCamiot.TAdt () happy_var_1 happy_var_2
	)}}

happyReduce_60 = happySpecReduce_1  11# happyReduction_60
happyReduction_60 happy_x_1
	 =  happyIn51
		 (AbsTinyCamiot.TBool ()
	)

happyReduce_61 = happySpecReduce_1  11# happyReduction_61
happyReduction_61 happy_x_1
	 =  happyIn51
		 (AbsTinyCamiot.TInt ()
	)

happyReduce_62 = happySpecReduce_1  11# happyReduction_62
happyReduction_62 happy_x_1
	 =  happyIn51
		 (AbsTinyCamiot.TFloat ()
	)

happyReduce_63 = happySpecReduce_3  11# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_2 of { (HappyWrap49 happy_var_2) -> 
	happyIn51
		 (happy_var_2
	)}

happyReduce_64 = happySpecReduce_1  12# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn52
		 (AbsTinyCamiot.TTupType () happy_var_1
	)}

happyReduce_65 = happySpecReduce_0  13# happyReduction_65
happyReduction_65  =  happyIn53
		 ([]
	)

happyReduce_66 = happySpecReduce_1  13# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	happyIn53
		 ((:[]) happy_var_1
	)}

happyReduce_67 = happySpecReduce_3  13# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	happyIn53
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_68 = happySpecReduce_0  14# happyReduction_68
happyReduction_68  =  happyIn54
		 ([]
	)

happyReduce_69 = happySpecReduce_2  14# happyReduction_69
happyReduction_69 happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	case happyOut54 happy_x_2 of { (HappyWrap54 happy_var_2) -> 
	happyIn54
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_70 = happySpecReduce_1  15# happyReduction_70
happyReduction_70 happy_x_1
	 =  case happyOut57 happy_x_1 of { (HappyWrap57 happy_var_1) -> 
	happyIn55
		 (AbsTinyCamiot.ETupExp () happy_var_1
	)}

happyReduce_71 = happySpecReduce_1  16# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	happyIn56
		 ((:[]) happy_var_1
	)}

happyReduce_72 = happySpecReduce_3  16# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	happyIn56
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_73 = happyReduce 6# 17# happyReduction_73
happyReduction_73 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	case happyOut80 happy_x_5 of { (HappyWrap80 happy_var_5) -> 
	happyIn57
		 (AbsTinyCamiot.ECase () happy_var_2 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_74 = happyReduce 6# 17# happyReduction_74
happyReduction_74 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut71 happy_x_2 of { (HappyWrap71 happy_var_2) -> 
	case happyOut57 happy_x_4 of { (HappyWrap57 happy_var_4) -> 
	case happyOut57 happy_x_6 of { (HappyWrap57 happy_var_6) -> 
	happyIn57
		 (AbsTinyCamiot.ELet () happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_75 = happyReduce 6# 17# happyReduction_75
happyReduction_75 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut71 happy_x_2 of { (HappyWrap71 happy_var_2) -> 
	case happyOut57 happy_x_4 of { (HappyWrap57 happy_var_4) -> 
	case happyOut57 happy_x_6 of { (HappyWrap57 happy_var_6) -> 
	happyIn57
		 (AbsTinyCamiot.ELetR () happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_76 = happyReduce 4# 17# happyReduction_76
happyReduction_76 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut71 happy_x_2 of { (HappyWrap71 happy_var_2) -> 
	case happyOut57 happy_x_4 of { (HappyWrap57 happy_var_4) -> 
	happyIn57
		 (AbsTinyCamiot.ELam () happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_77 = happyReduce 6# 17# happyReduction_77
happyReduction_77 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	case happyOut57 happy_x_4 of { (HappyWrap57 happy_var_4) -> 
	case happyOut57 happy_x_6 of { (HappyWrap57 happy_var_6) -> 
	happyIn57
		 (AbsTinyCamiot.EIf () happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_78 = happySpecReduce_1  17# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOut59 happy_x_1 of { (HappyWrap59 happy_var_1) -> 
	happyIn57
		 (happy_var_1
	)}

happyReduce_79 = happySpecReduce_2  18# happyReduction_79
happyReduction_79 happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	case happyOut64 happy_x_2 of { (HappyWrap64 happy_var_2) -> 
	happyIn58
		 (AbsTinyCamiot.EApp () happy_var_1 happy_var_2
	)}}

happyReduce_80 = happySpecReduce_2  18# happyReduction_80
happyReduction_80 happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_2 of { (HappyWrap64 happy_var_2) -> 
	happyIn58
		 (AbsTinyCamiot.ENot () happy_var_2
	)}

happyReduce_81 = happySpecReduce_1  18# happyReduction_81
happyReduction_81 happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	happyIn58
		 (happy_var_1
	)}

happyReduce_82 = happySpecReduce_3  19# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_1 of { (HappyWrap60 happy_var_1) -> 
	case happyOut59 happy_x_3 of { (HappyWrap59 happy_var_3) -> 
	happyIn59
		 (AbsTinyCamiot.EOr () happy_var_1 happy_var_3
	)}}

happyReduce_83 = happySpecReduce_1  19# happyReduction_83
happyReduction_83 happy_x_1
	 =  case happyOut60 happy_x_1 of { (HappyWrap60 happy_var_1) -> 
	happyIn59
		 (happy_var_1
	)}

happyReduce_84 = happySpecReduce_3  20# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut61 happy_x_1 of { (HappyWrap61 happy_var_1) -> 
	case happyOut60 happy_x_3 of { (HappyWrap60 happy_var_3) -> 
	happyIn60
		 (AbsTinyCamiot.EAnd () happy_var_1 happy_var_3
	)}}

happyReduce_85 = happySpecReduce_1  20# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut61 happy_x_1 of { (HappyWrap61 happy_var_1) -> 
	happyIn60
		 (happy_var_1
	)}

happyReduce_86 = happySpecReduce_3  21# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut61 happy_x_1 of { (HappyWrap61 happy_var_1) -> 
	case happyOut67 happy_x_2 of { (HappyWrap67 happy_var_2) -> 
	case happyOut62 happy_x_3 of { (HappyWrap62 happy_var_3) -> 
	happyIn61
		 (AbsTinyCamiot.ERel () happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_87 = happySpecReduce_1  21# happyReduction_87
happyReduction_87 happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	happyIn61
		 (happy_var_1
	)}

happyReduce_88 = happySpecReduce_3  22# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	case happyOut65 happy_x_2 of { (HappyWrap65 happy_var_2) -> 
	case happyOut63 happy_x_3 of { (HappyWrap63 happy_var_3) -> 
	happyIn62
		 (AbsTinyCamiot.EAdd () happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_89 = happySpecReduce_1  22# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOut63 happy_x_1 of { (HappyWrap63 happy_var_1) -> 
	happyIn62
		 (happy_var_1
	)}

happyReduce_90 = happySpecReduce_3  23# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut63 happy_x_1 of { (HappyWrap63 happy_var_1) -> 
	case happyOut66 happy_x_2 of { (HappyWrap66 happy_var_2) -> 
	case happyOut58 happy_x_3 of { (HappyWrap58 happy_var_3) -> 
	happyIn63
		 (AbsTinyCamiot.EMul () happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_91 = happySpecReduce_1  23# happyReduction_91
happyReduction_91 happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	happyIn63
		 (happy_var_1
	)}

happyReduce_92 = happySpecReduce_3  24# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	happyIn64
		 (AbsTinyCamiot.ETup () happy_var_2
	)}

happyReduce_93 = happySpecReduce_1  24# happyReduction_93
happyReduction_93 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn64
		 (AbsTinyCamiot.EVar () happy_var_1
	)}

happyReduce_94 = happySpecReduce_1  24# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn64
		 (AbsTinyCamiot.EUVar () happy_var_1
	)}

happyReduce_95 = happySpecReduce_1  24# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut70 happy_x_1 of { (HappyWrap70 happy_var_1) -> 
	happyIn64
		 (AbsTinyCamiot.EConst () happy_var_1
	)}

happyReduce_96 = happySpecReduce_3  24# happyReduction_96
happyReduction_96 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn64
		 (happy_var_2
	)}

happyReduce_97 = happySpecReduce_1  25# happyReduction_97
happyReduction_97 happy_x_1
	 =  happyIn65
		 (AbsTinyCamiot.Plus ()
	)

happyReduce_98 = happySpecReduce_1  25# happyReduction_98
happyReduction_98 happy_x_1
	 =  happyIn65
		 (AbsTinyCamiot.Minus ()
	)

happyReduce_99 = happySpecReduce_1  26# happyReduction_99
happyReduction_99 happy_x_1
	 =  happyIn66
		 (AbsTinyCamiot.Times ()
	)

happyReduce_100 = happySpecReduce_1  26# happyReduction_100
happyReduction_100 happy_x_1
	 =  happyIn66
		 (AbsTinyCamiot.Div ()
	)

happyReduce_101 = happySpecReduce_1  27# happyReduction_101
happyReduction_101 happy_x_1
	 =  happyIn67
		 (AbsTinyCamiot.LTC ()
	)

happyReduce_102 = happySpecReduce_1  27# happyReduction_102
happyReduction_102 happy_x_1
	 =  happyIn67
		 (AbsTinyCamiot.LEC ()
	)

happyReduce_103 = happySpecReduce_1  27# happyReduction_103
happyReduction_103 happy_x_1
	 =  happyIn67
		 (AbsTinyCamiot.GTC ()
	)

happyReduce_104 = happySpecReduce_1  27# happyReduction_104
happyReduction_104 happy_x_1
	 =  happyIn67
		 (AbsTinyCamiot.GEC ()
	)

happyReduce_105 = happySpecReduce_1  27# happyReduction_105
happyReduction_105 happy_x_1
	 =  happyIn67
		 (AbsTinyCamiot.EQC ()
	)

happyReduce_106 = happySpecReduce_0  28# happyReduction_106
happyReduction_106  =  happyIn68
		 ([]
	)

happyReduce_107 = happySpecReduce_2  28# happyReduction_107
happyReduction_107 happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_1 of { (HappyWrap57 happy_var_1) -> 
	case happyOut68 happy_x_2 of { (HappyWrap68 happy_var_2) -> 
	happyIn68
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_108 = happySpecReduce_1  29# happyReduction_108
happyReduction_108 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn69
		 (AbsTinyCamiot.Constructor () happy_var_1
	)}

happyReduce_109 = happySpecReduce_1  30# happyReduction_109
happyReduction_109 happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn70
		 (AbsTinyCamiot.CInt () happy_var_1
	)}

happyReduce_110 = happySpecReduce_1  30# happyReduction_110
happyReduction_110 happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	happyIn70
		 (AbsTinyCamiot.CFloat () happy_var_1
	)}

happyReduce_111 = happySpecReduce_1  30# happyReduction_111
happyReduction_111 happy_x_1
	 =  happyIn70
		 (AbsTinyCamiot.CTrue ()
	)

happyReduce_112 = happySpecReduce_1  30# happyReduction_112
happyReduction_112 happy_x_1
	 =  happyIn70
		 (AbsTinyCamiot.CFalse ()
	)

happyReduce_113 = happySpecReduce_1  30# happyReduction_113
happyReduction_113 happy_x_1
	 =  happyIn70
		 (AbsTinyCamiot.CNil ()
	)

happyReduce_114 = happySpecReduce_1  31# happyReduction_114
happyReduction_114 happy_x_1
	 =  case happyOut70 happy_x_1 of { (HappyWrap70 happy_var_1) -> 
	happyIn71
		 (AbsTinyCamiot.PConst () happy_var_1
	)}

happyReduce_115 = happySpecReduce_1  31# happyReduction_115
happyReduction_115 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn71
		 (AbsTinyCamiot.PVar () happy_var_1
	)}

happyReduce_116 = happySpecReduce_1  31# happyReduction_116
happyReduction_116 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn71
		 (AbsTinyCamiot.PZAdt () happy_var_1
	)}

happyReduce_117 = happyReduce 4# 31# happyReduction_117
happyReduction_117 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	case happyOut75 happy_x_3 of { (HappyWrap75 happy_var_3) -> 
	happyIn71
		 (AbsTinyCamiot.PNAdt () happy_var_2 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_118 = happySpecReduce_1  31# happyReduction_118
happyReduction_118 happy_x_1
	 =  happyIn71
		 (AbsTinyCamiot.PWild ()
	)

happyReduce_119 = happySpecReduce_2  31# happyReduction_119
happyReduction_119 happy_x_2
	happy_x_1
	 =  happyIn71
		 (AbsTinyCamiot.PNil ()
	)

happyReduce_120 = happySpecReduce_1  31# happyReduction_120
happyReduction_120 happy_x_1
	 =  case happyOut72 happy_x_1 of { (HappyWrap72 happy_var_1) -> 
	happyIn71
		 (happy_var_1
	)}

happyReduce_121 = happySpecReduce_3  32# happyReduction_121
happyReduction_121 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut77 happy_x_2 of { (HappyWrap77 happy_var_2) -> 
	happyIn72
		 (AbsTinyCamiot.PTup () happy_var_2
	)}

happyReduce_122 = happySpecReduce_1  32# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOut73 happy_x_1 of { (HappyWrap73 happy_var_1) -> 
	happyIn72
		 (happy_var_1
	)}

happyReduce_123 = happySpecReduce_3  33# happyReduction_123
happyReduction_123 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	case happyOut71 happy_x_3 of { (HappyWrap71 happy_var_3) -> 
	happyIn73
		 (AbsTinyCamiot.PLay () happy_var_1 happy_var_3
	)}}

happyReduce_124 = happySpecReduce_3  33# happyReduction_124
happyReduction_124 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut71 happy_x_2 of { (HappyWrap71 happy_var_2) -> 
	happyIn73
		 (happy_var_2
	)}

happyReduce_125 = happySpecReduce_1  34# happyReduction_125
happyReduction_125 happy_x_1
	 =  case happyOut71 happy_x_1 of { (HappyWrap71 happy_var_1) -> 
	happyIn74
		 (AbsTinyCamiot.PAdtPat () happy_var_1
	)}

happyReduce_126 = happySpecReduce_1  35# happyReduction_126
happyReduction_126 happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	happyIn75
		 ((:[]) happy_var_1
	)}

happyReduce_127 = happySpecReduce_2  35# happyReduction_127
happyReduction_127 happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	case happyOut75 happy_x_2 of { (HappyWrap75 happy_var_2) -> 
	happyIn75
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_128 = happySpecReduce_1  36# happyReduction_128
happyReduction_128 happy_x_1
	 =  case happyOut71 happy_x_1 of { (HappyWrap71 happy_var_1) -> 
	happyIn76
		 (AbsTinyCamiot.PTupPat () happy_var_1
	)}

happyReduce_129 = happySpecReduce_0  37# happyReduction_129
happyReduction_129  =  happyIn77
		 ([]
	)

happyReduce_130 = happySpecReduce_1  37# happyReduction_130
happyReduction_130 happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	happyIn77
		 ((:[]) happy_var_1
	)}

happyReduce_131 = happySpecReduce_3  37# happyReduction_131
happyReduction_131 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	case happyOut77 happy_x_3 of { (HappyWrap77 happy_var_3) -> 
	happyIn77
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_132 = happySpecReduce_0  38# happyReduction_132
happyReduction_132  =  happyIn78
		 ([]
	)

happyReduce_133 = happySpecReduce_2  38# happyReduction_133
happyReduction_133 happy_x_2
	happy_x_1
	 =  case happyOut71 happy_x_1 of { (HappyWrap71 happy_var_1) -> 
	case happyOut78 happy_x_2 of { (HappyWrap78 happy_var_2) -> 
	happyIn78
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_134 = happySpecReduce_3  39# happyReduction_134
happyReduction_134 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut71 happy_x_1 of { (HappyWrap71 happy_var_1) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	happyIn79
		 (AbsTinyCamiot.PM () happy_var_1 happy_var_3
	)}}

happyReduce_135 = happySpecReduce_1  40# happyReduction_135
happyReduction_135 happy_x_1
	 =  case happyOut79 happy_x_1 of { (HappyWrap79 happy_var_1) -> 
	happyIn80
		 ((:[]) happy_var_1
	)}

happyReduce_136 = happySpecReduce_3  40# happyReduction_136
happyReduction_136 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut79 happy_x_1 of { (HappyWrap79 happy_var_1) -> 
	case happyOut80 happy_x_3 of { (HappyWrap80 happy_var_3) -> 
	happyIn80
		 ((:) happy_var_1 happy_var_3
	)}}

happyNewToken action sts stk [] =
	happyDoAction 45# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TV happy_dollar_dollar) -> cont 41#;
	PT _ (TI happy_dollar_dollar) -> cont 42#;
	PT _ (TD happy_dollar_dollar) -> cont 43#;
	PT _ (T_UIdent happy_dollar_dollar) -> cont 44#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 45# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either String a -> (a -> Either String b) -> Either String b
happyThen = ((>>=))
happyReturn :: () => a -> Either String a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Either String a
happyError' = (\(tokens, _) -> happyError tokens)
pListDef tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap44 x') = happyOut44 x} in x'))

pDef tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap45 x') = happyOut45 x} in x'))

pConstructorDec tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap46 x') = happyOut46 x} in x'))

pListConstructorDec tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap47 x') = happyOut47 x} in x'))

pListIdent tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap48 x') = happyOut48 x} in x'))

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (let {(HappyWrap49 x') = happyOut49 x} in x'))

pType1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (let {(HappyWrap50 x') = happyOut50 x} in x'))

pType2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (let {(HappyWrap51 x') = happyOut51 x} in x'))

pTupType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (let {(HappyWrap52 x') = happyOut52 x} in x'))

pListTupType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (let {(HappyWrap53 x') = happyOut53 x} in x'))

pListType1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (let {(HappyWrap54 x') = happyOut54 x} in x'))

pTupExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (let {(HappyWrap55 x') = happyOut55 x} in x'))

pListTupExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (let {(HappyWrap56 x') = happyOut56 x} in x'))

pExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (let {(HappyWrap57 x') = happyOut57 x} in x'))

pExp6 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (let {(HappyWrap58 x') = happyOut58 x} in x'))

pExp1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (let {(HappyWrap59 x') = happyOut59 x} in x'))

pExp2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (let {(HappyWrap60 x') = happyOut60 x} in x'))

pExp3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (let {(HappyWrap61 x') = happyOut61 x} in x'))

pExp4 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (let {(HappyWrap62 x') = happyOut62 x} in x'))

pExp5 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (let {(HappyWrap63 x') = happyOut63 x} in x'))

pExp7 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (let {(HappyWrap64 x') = happyOut64 x} in x'))

pAddOp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (let {(HappyWrap65 x') = happyOut65 x} in x'))

pMulOp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (let {(HappyWrap66 x') = happyOut66 x} in x'))

pRelOp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (let {(HappyWrap67 x') = happyOut67 x} in x'))

pListExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (let {(HappyWrap68 x') = happyOut68 x} in x'))

pCon tks = happySomeParser where
 happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (let {(HappyWrap69 x') = happyOut69 x} in x'))

pConst tks = happySomeParser where
 happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (let {(HappyWrap70 x') = happyOut70 x} in x'))

pPat tks = happySomeParser where
 happySomeParser = happyThen (happyParse 27# tks) (\x -> happyReturn (let {(HappyWrap71 x') = happyOut71 x} in x'))

pPat1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 28# tks) (\x -> happyReturn (let {(HappyWrap72 x') = happyOut72 x} in x'))

pPat2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 29# tks) (\x -> happyReturn (let {(HappyWrap73 x') = happyOut73 x} in x'))

pAdtPat tks = happySomeParser where
 happySomeParser = happyThen (happyParse 30# tks) (\x -> happyReturn (let {(HappyWrap74 x') = happyOut74 x} in x'))

pListAdtPat tks = happySomeParser where
 happySomeParser = happyThen (happyParse 31# tks) (\x -> happyReturn (let {(HappyWrap75 x') = happyOut75 x} in x'))

pTupPat tks = happySomeParser where
 happySomeParser = happyThen (happyParse 32# tks) (\x -> happyReturn (let {(HappyWrap76 x') = happyOut76 x} in x'))

pListTupPat tks = happySomeParser where
 happySomeParser = happyThen (happyParse 33# tks) (\x -> happyReturn (let {(HappyWrap77 x') = happyOut77 x} in x'))

pListPat tks = happySomeParser where
 happySomeParser = happyThen (happyParse 34# tks) (\x -> happyReturn (let {(HappyWrap78 x') = happyOut78 x} in x'))

pPatMatch tks = happySomeParser where
 happySomeParser = happyThen (happyParse 35# tks) (\x -> happyReturn (let {(HappyWrap79 x') = happyOut79 x} in x'))

pListPatMatch tks = happySomeParser where
 happySomeParser = happyThen (happyParse 36# tks) (\x -> happyReturn (let {(HappyWrap80 x') = happyOut80 x} in x'))

happySeq = happyDontSeq


happyError :: [Token] -> Either String a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 10 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8371_0/ghc_2.h" #-}
































































































































































































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
