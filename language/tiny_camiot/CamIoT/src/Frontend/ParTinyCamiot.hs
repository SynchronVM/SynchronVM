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
newtype HappyWrap34 = HappyWrap34 (AbsTinyCamiot.Ident)
happyIn34 :: (AbsTinyCamiot.Ident) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 (Integer)
happyIn35 :: (Integer) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 (Double)
happyIn36 :: (Double) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 (AbsTinyCamiot.UIdent)
happyIn37 :: (AbsTinyCamiot.UIdent) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 ([AbsTinyCamiot.Def ()])
happyIn38 :: ([AbsTinyCamiot.Def ()]) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 ((AbsTinyCamiot.Def ()))
happyIn39 :: ((AbsTinyCamiot.Def ())) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 ((AbsTinyCamiot.ConstructorDec ()))
happyIn40 :: ((AbsTinyCamiot.ConstructorDec ())) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 ([AbsTinyCamiot.ConstructorDec ()])
happyIn41 :: ([AbsTinyCamiot.ConstructorDec ()]) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 ([AbsTinyCamiot.Ident])
happyIn42 :: ([AbsTinyCamiot.Ident]) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 ((AbsTinyCamiot.Type ()))
happyIn43 :: ((AbsTinyCamiot.Type ())) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 (AbsTinyCamiot.Type ())
happyIn44 :: (AbsTinyCamiot.Type ()) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 (AbsTinyCamiot.Type ())
happyIn45 :: (AbsTinyCamiot.Type ()) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 ([AbsTinyCamiot.Type ()])
happyIn46 :: ([AbsTinyCamiot.Type ()]) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 ((AbsTinyCamiot.TupExp ()))
happyIn47 :: ((AbsTinyCamiot.TupExp ())) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 ([AbsTinyCamiot.TupExp ()])
happyIn48 :: ([AbsTinyCamiot.TupExp ()]) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 ((AbsTinyCamiot.Exp ()))
happyIn49 :: ((AbsTinyCamiot.Exp ())) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 (AbsTinyCamiot.Exp ())
happyIn50 :: (AbsTinyCamiot.Exp ()) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 (AbsTinyCamiot.Exp ())
happyIn51 :: (AbsTinyCamiot.Exp ()) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 (AbsTinyCamiot.Exp ())
happyIn52 :: (AbsTinyCamiot.Exp ()) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 (AbsTinyCamiot.Exp ())
happyIn53 :: (AbsTinyCamiot.Exp ()) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 (AbsTinyCamiot.Exp ())
happyIn54 :: (AbsTinyCamiot.Exp ()) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
newtype HappyWrap55 = HappyWrap55 (AbsTinyCamiot.Exp ())
happyIn55 :: (AbsTinyCamiot.Exp ()) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap55 x)
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> HappyWrap55
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
newtype HappyWrap56 = HappyWrap56 (AbsTinyCamiot.Exp ())
happyIn56 :: (AbsTinyCamiot.Exp ()) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap56 x)
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> HappyWrap56
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
newtype HappyWrap57 = HappyWrap57 ((AbsTinyCamiot.AddOp ()))
happyIn57 :: ((AbsTinyCamiot.AddOp ())) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap57 x)
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> HappyWrap57
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
newtype HappyWrap58 = HappyWrap58 ((AbsTinyCamiot.MulOp ()))
happyIn58 :: ((AbsTinyCamiot.MulOp ())) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap58 x)
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> HappyWrap58
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
newtype HappyWrap59 = HappyWrap59 ((AbsTinyCamiot.RelOp ()))
happyIn59 :: ((AbsTinyCamiot.RelOp ())) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap59 x)
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> HappyWrap59
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
newtype HappyWrap60 = HappyWrap60 ([AbsTinyCamiot.Exp ()])
happyIn60 :: ([AbsTinyCamiot.Exp ()]) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap60 x)
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> HappyWrap60
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
newtype HappyWrap61 = HappyWrap61 ((AbsTinyCamiot.Con ()))
happyIn61 :: ((AbsTinyCamiot.Con ())) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap61 x)
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> HappyWrap61
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
newtype HappyWrap62 = HappyWrap62 ((AbsTinyCamiot.Const ()))
happyIn62 :: ((AbsTinyCamiot.Const ())) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap62 x)
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> HappyWrap62
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
newtype HappyWrap63 = HappyWrap63 ((AbsTinyCamiot.Pat ()))
happyIn63 :: ((AbsTinyCamiot.Pat ())) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap63 x)
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> HappyWrap63
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
newtype HappyWrap64 = HappyWrap64 (AbsTinyCamiot.Pat ())
happyIn64 :: (AbsTinyCamiot.Pat ()) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap64 x)
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> HappyWrap64
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
newtype HappyWrap65 = HappyWrap65 (AbsTinyCamiot.Pat ())
happyIn65 :: (AbsTinyCamiot.Pat ()) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap65 x)
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> HappyWrap65
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
newtype HappyWrap66 = HappyWrap66 ([AbsTinyCamiot.Pat ()])
happyIn66 :: ([AbsTinyCamiot.Pat ()]) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap66 x)
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> HappyWrap66
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
newtype HappyWrap67 = HappyWrap67 ((AbsTinyCamiot.PatMatch ()))
happyIn67 :: ((AbsTinyCamiot.PatMatch ())) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap67 x)
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> HappyWrap67
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
newtype HappyWrap68 = HappyWrap68 ([AbsTinyCamiot.PatMatch ()])
happyIn68 :: ([AbsTinyCamiot.PatMatch ()]) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap68 x)
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> HappyWrap68
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x80\x04\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\xe0\xa4\x81\x07\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x38\x69\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x40\x03\x00\x4e\x1a\x78\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x80\x01\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x60\x00\x80\x03\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x18\x00\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x40\x03\x00\x06\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x80\x01\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x60\x00\x80\x03\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x18\x00\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x38\x69\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x80\x01\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x60\x01\x80\x07\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x80\x05\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x60\x01\x80\x07\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x58\x00\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x16\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x80\x05\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x58\x00\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\xd8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x06\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x80\x93\x06\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x60\x01\x80\x07\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x38\x69\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x40\x03\x00\x4e\x1a\x78\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x80\x05\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x60\x01\x80\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x03\x00\x4e\x1a\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x80\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x02\x16\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x80\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x80\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x80\x93\x06\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x60\x00\x80\x03\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x18\x00\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x40\x03\x00\x06\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x18\x00\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x80\x01\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x80\x05\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x16\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x80\x93\x06\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x58\x00\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x03\x00\x4e\x1a\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\xe0\xa4\x81\x07\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x38\x69\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x40\x03\x00\x4e\x1a\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x38\x69\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x58\x00\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x40\x03\x00\x4e\x1a\x78\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x80\x93\x06\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\xe0\xa4\x81\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pListDef","%start_pDef","%start_pConstructorDec","%start_pListConstructorDec","%start_pListIdent","%start_pType","%start_pType1","%start_pType2","%start_pListType","%start_pTupExp","%start_pListTupExp","%start_pExp","%start_pExp1","%start_pExp2","%start_pExp3","%start_pExp4","%start_pExp5","%start_pExp6","%start_pExp7","%start_pAddOp","%start_pMulOp","%start_pRelOp","%start_pListExp","%start_pCon","%start_pConst","%start_pPat","%start_pPat1","%start_pPat2","%start_pListPat","%start_pPatMatch","%start_pListPatMatch","Ident","Integer","Double","UIdent","ListDef","Def","ConstructorDec","ListConstructorDec","ListIdent","Type","Type1","Type2","ListType","TupExp","ListTupExp","Exp","Exp1","Exp2","Exp3","Exp4","Exp5","Exp6","Exp7","AddOp","MulOp","RelOp","ListExp","Con","Const","Pat","Pat1","Pat2","ListPat","PatMatch","ListPatMatch","'!'","'&&'","'('","'()'","')'","'*'","'+'","','","'-'","'->'","'/'","':'","';'","'<'","'<='","'='","'=='","'>'","'>='","'False'","'True'","'\\\\'","'_'","'as'","'case'","'data'","'else'","'if'","'in'","'let'","'letrec'","'of'","'then'","'where'","'{'","'||'","'}'","L_Ident","L_integ","L_doubl","L_UIdent","%eof"]
        bit_start = st * 110
        bit_end = (st + 1) * 110
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..109]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xe9\xff\xe9\xff\xde\xff\xde\xff\xe7\xff\x20\x00\x20\x00\x20\x00\x20\x00\x01\x00\x01\x00\x01\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x4d\x00\x0b\x00\x47\x00\xea\x01\x01\x00\xfd\xff\x53\x00\x37\x00\x06\x00\x16\x00\x37\x00\x37\x00\x37\x00\x18\x00\x00\x00\x21\x00\x00\x00\x00\x00\x37\x00\x00\x00\x4e\x00\x00\x00\x00\x00\x56\x00\x3a\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x37\x00\x3a\x00\x58\x00\x3b\x00\x37\x00\x3b\x00\x37\x00\x3b\x00\x3b\x00\x00\x00\x3b\x00\x00\x00\x01\x00\x00\x00\x07\x00\x4e\x03\x0b\x00\x47\x00\x00\x00\x00\x00\x3b\x00\x01\x00\x00\x00\x4d\x00\x01\x00\x37\x00\x01\x00\x01\x00\x37\x00\x37\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x3b\x00\x01\x00\x3b\x00\xfb\xff\x3c\x00\x21\x01\x3b\x00\x3b\x00\x3b\x00\x79\x00\x66\x00\x00\x00\x66\x00\x00\x00\x20\x00\x20\x00\x67\x00\x90\x00\x7b\x00\x20\x00\x7b\x00\x7b\x00\x7b\x00\x80\x00\x82\x00\xa6\x00\xaa\x00\x9f\x00\x9f\x00\x33\x00\x9f\x00\xf4\x00\x04\x01\x24\x01\xe9\xff\x1f\x01\x36\x01\x20\x00\x1e\x01\x20\x00\x00\x00\x5d\x01\x20\x00\x20\x00\x00\x00\x00\x00\x01\x00\x0d\x00\x0d\x00\x0d\x00\x71\x01\x67\x01\x74\x01\x64\x01\x66\x01\x86\x01\x90\x01\x9c\x01\x00\x00\x00\x00\x0d\x00\x00\x00\x0d\x00\x00\x00\x6a\x00\x9d\x01\x37\x00\x00\x00\x00\x00\x37\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x6e\x01\x01\x00\x01\x00\x01\x00\x00\x00\x47\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x81\x01\x00\x00\x8b\x01\x00\x00\x92\x01\xa1\x01\xa4\x01\x37\x00\x00\x00\xbb\x01\x00\x00\xa5\x01\x01\x00\x01\x00\x01\x00\xa8\x01\xa9\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xe4\x01\x13\x00\xa1\x00\x8c\x01\x38\x00\x77\x03\xa3\x03\xa7\x03\x62\x03\x56\x01\x11\x01\xc4\x01\xef\x02\x1f\x03\x36\x03\x06\x03\x48\x01\x96\x01\x78\x01\xb9\x01\xba\x01\xca\x01\x6d\x01\x09\x00\x81\x00\xc1\x00\x4f\x00\x1e\x00\xa0\x00\x8c\x00\x69\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xae\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb3\x00\x00\x00\x00\x00\x00\x00\xd9\x00\x00\x00\xe5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8a\x01\x00\x00\xfc\x02\xe7\x01\xf8\x01\xf9\x01\x00\x00\x00\x00\x00\x00\xa7\x01\x00\x00\xcb\x01\x28\x01\xe9\x00\xdb\x01\xf2\x01\xed\x00\xf1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x02\x00\x00\xf9\x01\xfb\x01\xf7\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x66\x03\x73\x03\x00\x00\x00\x00\x00\x00\x83\x03\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb8\x00\x00\x00\x10\x02\x00\x00\x00\x00\xfa\x01\x89\x00\x00\x00\x87\x03\xaa\x01\x93\x03\x00\x00\x00\x00\xa5\x03\x97\x03\x00\x00\x00\x00\x3f\x01\x42\x03\x5f\x01\xb3\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x03\x00\x00\x13\x03\x00\x00\x00\x00\x00\x00\xf9\x00\x00\x00\x00\x00\x76\x00\x20\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x37\x02\x00\x00\x4e\x02\x65\x02\x7c\x02\x00\x00\xfc\x01\xfe\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x93\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7d\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaa\x02\xc1\x02\xd8\x02\xdf\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xdc\xff\x00\x00\x00\x00\xd5\xff\xd2\xff\x00\x00\x00\x00\x00\x00\xc9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x91\xff\x00\x00\x8e\xff\x00\x00\xe0\xff\x9a\xff\xa0\xff\x9f\xff\x91\xff\x9b\xff\x00\x00\x96\xff\x94\xff\x8d\xff\x00\x00\x00\x00\x9c\xff\x9d\xff\x9e\xff\x98\xff\xdf\xff\xde\xff\xdd\xff\x00\x00\x91\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa1\xff\x00\x00\xaf\xff\xa3\xff\xbd\xff\xba\xff\xb8\xff\xb6\xff\xb4\xff\xb2\xff\xb0\xff\x00\x00\xa3\xff\xae\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa8\xff\xa7\xff\xa4\xff\xa6\xff\xa5\xff\x00\x00\xaa\xff\xa9\xff\x00\x00\xac\xff\xab\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\xff\x00\x00\xc7\xff\x00\x00\xcc\xff\xc9\xff\xc9\xff\xcf\xff\xcd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd2\xff\x00\x00\x00\x00\xd4\xff\x00\x00\x00\x00\x91\xff\x00\x00\x00\x00\x00\x00\xdb\xff\xdc\xff\xd2\xff\x00\x00\x00\x00\xd5\xff\x00\x00\xd1\xff\x00\x00\x00\x00\x00\x00\xc8\xff\xcb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc7\xff\xb1\xff\xbe\xff\x00\x00\xbc\xff\x00\x00\xa2\xff\x00\x00\x00\x00\x00\x00\x90\xff\x97\xff\x8e\xff\x00\x00\x99\xff\x8f\xff\x8c\xff\x93\xff\x92\xff\x00\x00\xbb\xff\xb9\xff\xad\xff\xc4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb3\xff\xb5\xff\xb7\xff\xc5\xff\xd0\xff\xce\xff\xca\xff\xd6\xff\xd3\xff\xd8\xff\x00\x00\x00\x00\xda\xff\x00\x00\xd9\xff\x00\x00\x00\x00\x00\x00\x8e\xff\xc0\xff\x00\x00\x95\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd5\xff\x00\x00\xc1\xff\xc2\xff\xbf\xff\xc3\xff\xd7\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x06\x00\x01\x00\x1a\x00\x03\x00\x04\x00\x0b\x00\x29\x00\x01\x00\x03\x00\x03\x00\x04\x00\x03\x00\x26\x00\x01\x00\x26\x00\x03\x00\x04\x00\x07\x00\x00\x00\x09\x00\x14\x00\x15\x00\x16\x00\x05\x00\x03\x00\x19\x00\x14\x00\x15\x00\x1c\x00\x00\x00\x1e\x00\x1f\x00\x14\x00\x15\x00\x03\x00\x1b\x00\x2a\x00\x29\x00\x26\x00\x27\x00\x28\x00\x29\x00\x24\x00\x26\x00\x26\x00\x27\x00\x28\x00\x03\x00\x04\x00\x05\x00\x26\x00\x27\x00\x28\x00\x03\x00\x04\x00\x00\x00\x18\x00\x03\x00\x04\x00\x26\x00\x1f\x00\x26\x00\x0c\x00\x08\x00\x14\x00\x15\x00\x07\x00\x17\x00\x09\x00\x26\x00\x14\x00\x15\x00\x29\x00\x17\x00\x14\x00\x15\x00\x06\x00\x17\x00\x00\x00\x03\x00\x04\x00\x0b\x00\x26\x00\x27\x00\x28\x00\x29\x00\x04\x00\x0a\x00\x26\x00\x27\x00\x28\x00\x29\x00\x26\x00\x27\x00\x28\x00\x29\x00\x14\x00\x15\x00\x0d\x00\x2a\x00\x2a\x00\x2a\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1e\x00\x1f\x00\x05\x00\x18\x00\x0a\x00\x08\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x27\x00\x28\x00\x00\x00\x00\x00\x01\x00\x02\x00\x03\x00\x08\x00\x01\x00\x02\x00\x08\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x21\x00\x22\x00\x00\x00\x01\x00\x02\x00\x03\x00\x2a\x00\x08\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x06\x00\x21\x00\x22\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x1c\x00\x21\x00\x22\x00\x00\x00\x01\x00\x02\x00\x03\x00\x03\x00\x2a\x00\x26\x00\x06\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x2a\x00\x21\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x2a\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x29\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\x2a\x00\x0e\x00\x0f\x00\x0d\x00\x11\x00\x12\x00\x13\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\x26\x00\x10\x00\x29\x00\x00\x00\x01\x00\x02\x00\x2a\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x05\x00\x0d\x00\x1c\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\x14\x00\x15\x00\x16\x00\x05\x00\x10\x00\x00\x00\x01\x00\x02\x00\x1c\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x10\x00\x21\x00\x20\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x16\x00\x03\x00\x0a\x00\x23\x00\x06\x00\x07\x00\x1c\x00\x05\x00\x00\x00\x01\x00\x02\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x05\x00\x05\x00\x22\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x15\x00\x16\x00\x03\x00\x23\x00\x1d\x00\x06\x00\x07\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x1d\x00\x1b\x00\x05\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x15\x00\x16\x00\x25\x00\x00\x00\x01\x00\x02\x00\x25\x00\x1c\x00\x17\x00\x29\x00\x18\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\x16\x00\x03\x00\x19\x00\x00\x00\x06\x00\x07\x00\x1c\x00\x04\x00\x05\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\x0e\x00\x0f\x00\x00\x00\x11\x00\x12\x00\x13\x00\x04\x00\x05\x00\x19\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\x17\x00\x19\x00\x18\x00\x17\x00\x03\x00\x18\x00\x17\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x1b\x00\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x1c\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x1c\x00\x13\x00\x14\x00\x15\x00\x16\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x1c\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x1c\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x1c\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x1c\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\xff\xff\xff\xff\xff\xff\x02\x00\xff\xff\x1c\x00\xff\xff\xff\xff\x13\x00\x14\x00\x15\x00\x16\x00\xff\xff\xff\xff\xff\xff\x0e\x00\x0f\x00\x1c\x00\x11\x00\x12\x00\x13\x00\x00\x00\xff\xff\xff\xff\x03\x00\x00\x00\xff\xff\xff\xff\x03\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\xff\xff\xff\xff\x03\x00\x00\x00\xff\xff\xff\xff\x03\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\xff\xff\xff\xff\x03\x00\x00\x00\xff\xff\xff\xff\x03\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x00\x00\xff\xff\xff\xff\x03\x00\x00\x00\xff\xff\xff\xff\x03\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x00\x00\xff\xff\x00\x00\x03\x00\x00\x00\x03\x00\xff\xff\x03\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x0a\x00\x0b\x00\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x5a\x00\x4c\x00\x7e\x00\x4d\x00\x2d\x00\x5b\x00\x33\x00\x4c\x00\x3b\x00\x60\x00\x2d\x00\x3d\x00\x21\x00\x4c\x00\x21\x00\x60\x00\x2d\x00\x5d\x00\x7b\x00\x5e\x00\x2e\x00\x2f\x00\x4e\x00\x7c\x00\x39\x00\x4f\x00\x2e\x00\x2f\x00\x50\x00\x36\x00\x51\x00\x52\x00\x2e\x00\x2f\x00\x72\x00\x3e\x00\xff\xff\x33\x00\x21\x00\x31\x00\x32\x00\x33\x00\x9d\x00\x21\x00\x21\x00\x31\x00\x32\x00\x2c\x00\x2d\x00\xa3\x00\x21\x00\x31\x00\x32\x00\x2c\x00\x2d\x00\x75\x00\xa1\x00\x2c\x00\x2d\x00\x21\x00\x37\x00\x21\x00\x84\x00\x76\x00\x2e\x00\x2f\x00\x5d\x00\x30\x00\x5e\x00\x21\x00\x2e\x00\x2f\x00\x33\x00\x30\x00\x2e\x00\x2f\x00\x5a\x00\x30\x00\x36\x00\x60\x00\x2d\x00\x5b\x00\x21\x00\x31\x00\x32\x00\x33\x00\x2d\x00\xa5\x00\x21\x00\x31\x00\x32\x00\x33\x00\x21\x00\x31\x00\x32\x00\x33\x00\x2e\x00\x2f\x00\xa4\x00\xff\xff\xff\xff\xff\xff\x2e\x00\x2f\x00\x21\x00\x22\x00\x23\x00\x24\x00\x39\x00\x28\x00\xaa\x00\xa1\x00\x8a\x00\xab\x00\x21\x00\x31\x00\x32\x00\x21\x00\x22\x00\x23\x00\x24\x00\x31\x00\x32\x00\x75\x00\x21\x00\x22\x00\x23\x00\x24\x00\x8d\x00\x22\x00\x23\x00\x86\x00\x25\x00\x26\x00\x27\x00\x28\x00\x75\x00\x29\x00\x2a\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\xbf\x00\x25\x00\x26\x00\x27\x00\x28\x00\x89\x00\x29\x00\xa7\x00\x25\x00\x26\x00\x27\x00\x28\x00\x3c\x00\x29\x00\xca\x00\x21\x00\x22\x00\x23\x00\x24\x00\x77\x00\xff\xff\x21\x00\x7a\x00\x25\x00\x26\x00\x27\x00\x28\x00\xff\xff\x33\x00\x21\x00\x22\x00\x23\x00\x24\x00\x86\x00\x21\x00\x22\x00\x23\x00\x24\x00\x85\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x34\x00\x27\x00\x28\x00\x35\x00\x21\x00\x22\x00\x23\x00\x24\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x25\x00\x34\x00\x27\x00\x28\x00\xa5\x00\x25\x00\x34\x00\x27\x00\x28\x00\xa1\x00\x25\x00\x34\x00\x27\x00\x28\x00\x82\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x3b\x00\x27\x00\x28\x00\x25\x00\x9e\x00\x27\x00\x28\x00\x21\x00\x22\x00\x23\x00\x24\x00\x21\x00\x22\x00\x23\x00\x24\x00\x21\x00\x22\x00\x23\x00\x24\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x9f\x00\x27\x00\x28\x00\x21\x00\x22\x00\x23\x00\x24\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x9e\x00\x27\x00\x28\x00\x25\x00\x95\x00\x27\x00\x28\x00\x25\x00\x92\x00\x27\x00\x28\x00\x25\x00\x91\x00\x27\x00\x28\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x25\x00\xa8\x00\x27\x00\x28\x00\x25\x00\xc8\x00\x27\x00\x28\x00\x33\x00\x67\x00\x68\x00\x69\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\xff\xff\x54\x00\x55\x00\x81\x00\x56\x00\x57\x00\x58\x00\x67\x00\x96\x00\x97\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\x21\x00\xbf\x00\x33\x00\x3f\x00\x22\x00\x23\x00\xff\xff\x67\x00\xb7\x00\x69\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\x61\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\xbb\x00\x6a\x00\x4a\x00\x69\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\xb5\x00\x46\x00\x47\x00\xae\x00\xb4\x00\x3f\x00\x22\x00\x23\x00\x4a\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\xb3\x00\xb2\x00\xb1\x00\x48\x00\x49\x00\x4a\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x5e\x00\x77\x00\xb0\x00\xc7\x00\x78\x00\x79\x00\x4a\x00\xaf\x00\x3f\x00\x22\x00\x23\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\xae\x00\xaa\x00\xc2\x00\x9d\x00\x49\x00\x4a\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x60\x00\x47\x00\x77\x00\xcf\x00\xce\x00\x78\x00\xbc\x00\x4a\x00\x3f\x00\x22\x00\x23\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\xcd\x00\xcc\x00\xca\x00\x99\x00\x49\x00\x4a\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\xb4\x00\x47\x00\xd4\x00\x3f\x00\x22\x00\x23\x00\xd5\x00\x4a\x00\x5b\x00\x33\x00\x58\x00\x66\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\x98\x00\x77\x00\x52\x00\x7b\x00\x78\x00\xcf\x00\x4a\x00\x7e\x00\x7f\x00\x94\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\x54\x00\x55\x00\x7b\x00\x56\x00\x57\x00\x58\x00\xc0\x00\x7f\x00\x8d\x00\x93\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\x8e\x00\x8d\x00\x8f\x00\x8e\x00\x81\x00\x8f\x00\x8e\x00\x00\x00\x00\x00\x90\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa6\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc7\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc5\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc3\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd2\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x3d\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x00\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x22\x00\x23\x00\x65\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x00\x00\x00\x00\x4a\x00\x9b\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x00\x00\x00\x00\x4a\x00\x62\x00\x45\x00\x46\x00\x47\x00\x00\x00\x00\x00\x3f\x00\x22\x00\x23\x00\x4a\x00\xab\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x00\x00\x3f\x00\x22\x00\x23\x00\x00\x00\x4a\x00\x64\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x00\x00\x00\x00\x4a\x00\xac\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x3f\x00\x22\x00\x23\x00\x00\x00\x00\x00\x4a\x00\x63\x00\x44\x00\x45\x00\x46\x00\x47\x00\x00\x00\x00\x00\x00\x00\x9b\x00\x00\x00\x4a\x00\x00\x00\x00\x00\xb6\x00\x45\x00\x46\x00\x47\x00\x00\x00\x00\x00\x00\x00\x54\x00\x55\x00\x4a\x00\x56\x00\x57\x00\x58\x00\x6b\x00\x00\x00\x00\x00\x6c\x00\x6b\x00\x00\x00\x00\x00\x6c\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x6d\x00\x6e\x00\x6f\x00\x8b\x00\x6b\x00\x00\x00\x00\x00\x6c\x00\x6b\x00\x00\x00\x00\x00\x6c\x00\x00\x00\x6d\x00\x6e\x00\x6f\x00\x8a\x00\x74\x00\x6e\x00\x6f\x00\x6b\x00\x00\x00\x00\x00\x6c\x00\x6b\x00\x00\x00\x00\x00\x6c\x00\x00\x00\x87\x00\x6e\x00\x6f\x00\x00\x00\xbd\x00\x6e\x00\x6f\x00\x6b\x00\x00\x00\x00\x00\x6c\x00\x6b\x00\x00\x00\x00\x00\x6c\x00\x00\x00\xbb\x00\x6e\x00\x6f\x00\x00\x00\xb8\x00\x6e\x00\x6f\x00\x6b\x00\x00\x00\x6b\x00\x6c\x00\x6b\x00\x6c\x00\x00\x00\x6c\x00\x00\x00\x00\x00\x73\x00\x6f\x00\xb9\x00\x6f\x00\x00\x00\x72\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (31, 115) [
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
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
	(115 , happyReduce_115)
	]

happy_n_terms = 43 :: Int
happy_n_nonterms = 35 :: Int

happyReduce_31 = happySpecReduce_1  0# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn34
		 (AbsTinyCamiot.Ident happy_var_1
	)}

happyReduce_32 = happySpecReduce_1  1# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn35
		 ((read (happy_var_1)) :: Integer
	)}

happyReduce_33 = happySpecReduce_1  2# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TD happy_var_1)) -> 
	happyIn36
		 ((read (happy_var_1)) :: Double
	)}

happyReduce_34 = happySpecReduce_1  3# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_UIdent happy_var_1)) -> 
	happyIn37
		 (AbsTinyCamiot.UIdent happy_var_1
	)}

happyReduce_35 = happySpecReduce_0  4# happyReduction_35
happyReduction_35  =  happyIn38
		 ([]
	)

happyReduce_36 = happySpecReduce_1  4# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	happyIn38
		 ((:[]) happy_var_1
	)}

happyReduce_37 = happySpecReduce_3  4# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	case happyOut38 happy_x_3 of { (HappyWrap38 happy_var_3) -> 
	happyIn38
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_38 = happyReduce 4# 5# happyReduction_38
happyReduction_38 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	case happyOut66 happy_x_2 of { (HappyWrap66 happy_var_2) -> 
	case happyOut49 happy_x_4 of { (HappyWrap49 happy_var_4) -> 
	happyIn39
		 (AbsTinyCamiot.DEquation () happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_39 = happySpecReduce_3  5# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	case happyOut43 happy_x_3 of { (HappyWrap43 happy_var_3) -> 
	happyIn39
		 (AbsTinyCamiot.DTypeSig () happy_var_1 happy_var_3
	)}}

happyReduce_40 = happyReduce 7# 5# happyReduction_40
happyReduction_40 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_2 of { (HappyWrap37 happy_var_2) -> 
	case happyOut42 happy_x_3 of { (HappyWrap42 happy_var_3) -> 
	case happyOut41 happy_x_6 of { (HappyWrap41 happy_var_6) -> 
	happyIn39
		 (AbsTinyCamiot.DDataDec () happy_var_2 happy_var_3 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_41 = happySpecReduce_3  6# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	case happyOut43 happy_x_3 of { (HappyWrap43 happy_var_3) -> 
	happyIn40
		 (AbsTinyCamiot.ConstDec () happy_var_1 happy_var_3
	)}}

happyReduce_42 = happySpecReduce_0  7# happyReduction_42
happyReduction_42  =  happyIn41
		 ([]
	)

happyReduce_43 = happySpecReduce_1  7# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn41
		 ((:[]) happy_var_1
	)}

happyReduce_44 = happySpecReduce_3  7# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	case happyOut41 happy_x_3 of { (HappyWrap41 happy_var_3) -> 
	happyIn41
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_45 = happySpecReduce_0  8# happyReduction_45
happyReduction_45  =  happyIn42
		 ([]
	)

happyReduce_46 = happySpecReduce_2  8# happyReduction_46
happyReduction_46 happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	case happyOut42 happy_x_2 of { (HappyWrap42 happy_var_2) -> 
	happyIn42
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_47 = happySpecReduce_3  9# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	case happyOut43 happy_x_3 of { (HappyWrap43 happy_var_3) -> 
	happyIn43
		 (AbsTinyCamiot.TLam () happy_var_1 happy_var_3
	)}}

happyReduce_48 = happySpecReduce_1  9# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	happyIn43
		 (happy_var_1
	)}

happyReduce_49 = happySpecReduce_3  10# happyReduction_49
happyReduction_49 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	case happyOut44 happy_x_3 of { (HappyWrap44 happy_var_3) -> 
	happyIn44
		 (AbsTinyCamiot.TPair () happy_var_1 happy_var_3
	)}}

happyReduce_50 = happySpecReduce_1  10# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	happyIn44
		 (happy_var_1
	)}

happyReduce_51 = happySpecReduce_1  11# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn45
		 (AbsTinyCamiot.TVar () happy_var_1
	)}

happyReduce_52 = happySpecReduce_2  11# happyReduction_52
happyReduction_52 happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	case happyOut46 happy_x_2 of { (HappyWrap46 happy_var_2) -> 
	happyIn45
		 (AbsTinyCamiot.TAdt () happy_var_1 happy_var_2
	)}}

happyReduce_53 = happySpecReduce_3  11# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	happyIn45
		 (happy_var_2
	)}

happyReduce_54 = happySpecReduce_0  12# happyReduction_54
happyReduction_54  =  happyIn46
		 ([]
	)

happyReduce_55 = happySpecReduce_2  12# happyReduction_55
happyReduction_55 happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	case happyOut46 happy_x_2 of { (HappyWrap46 happy_var_2) -> 
	happyIn46
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_56 = happySpecReduce_1  13# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn47
		 (AbsTinyCamiot.ETupExp () happy_var_1
	)}

happyReduce_57 = happySpecReduce_1  14# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	happyIn48
		 ((:[]) happy_var_1
	)}

happyReduce_58 = happySpecReduce_3  14# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	case happyOut48 happy_x_3 of { (HappyWrap48 happy_var_3) -> 
	happyIn48
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_59 = happySpecReduce_3  15# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_2 of { (HappyWrap48 happy_var_2) -> 
	happyIn49
		 (AbsTinyCamiot.ETup () happy_var_2
	)}

happyReduce_60 = happyReduce 6# 15# happyReduction_60
happyReduction_60 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut49 happy_x_2 of { (HappyWrap49 happy_var_2) -> 
	case happyOut68 happy_x_5 of { (HappyWrap68 happy_var_5) -> 
	happyIn49
		 (AbsTinyCamiot.ECase () happy_var_2 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_61 = happyReduce 6# 15# happyReduction_61
happyReduction_61 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut63 happy_x_2 of { (HappyWrap63 happy_var_2) -> 
	case happyOut49 happy_x_4 of { (HappyWrap49 happy_var_4) -> 
	case happyOut49 happy_x_6 of { (HappyWrap49 happy_var_6) -> 
	happyIn49
		 (AbsTinyCamiot.ELet () happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_62 = happyReduce 6# 15# happyReduction_62
happyReduction_62 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut63 happy_x_2 of { (HappyWrap63 happy_var_2) -> 
	case happyOut49 happy_x_4 of { (HappyWrap49 happy_var_4) -> 
	case happyOut49 happy_x_6 of { (HappyWrap49 happy_var_6) -> 
	happyIn49
		 (AbsTinyCamiot.ELetR () happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_63 = happyReduce 4# 15# happyReduction_63
happyReduction_63 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut63 happy_x_2 of { (HappyWrap63 happy_var_2) -> 
	case happyOut49 happy_x_4 of { (HappyWrap49 happy_var_4) -> 
	happyIn49
		 (AbsTinyCamiot.ELam () happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_64 = happyReduce 6# 15# happyReduction_64
happyReduction_64 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut49 happy_x_2 of { (HappyWrap49 happy_var_2) -> 
	case happyOut49 happy_x_4 of { (HappyWrap49 happy_var_4) -> 
	case happyOut49 happy_x_6 of { (HappyWrap49 happy_var_6) -> 
	happyIn49
		 (AbsTinyCamiot.EIf () happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_65 = happySpecReduce_2  15# happyReduction_65
happyReduction_65 happy_x_2
	happy_x_1
	 =  case happyOut61 happy_x_1 of { (HappyWrap61 happy_var_1) -> 
	case happyOut60 happy_x_2 of { (HappyWrap60 happy_var_2) -> 
	happyIn49
		 (AbsTinyCamiot.ECon () happy_var_1 happy_var_2
	)}}

happyReduce_66 = happySpecReduce_1  15# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	happyIn49
		 (happy_var_1
	)}

happyReduce_67 = happySpecReduce_2  16# happyReduction_67
happyReduction_67 happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	case happyOut50 happy_x_2 of { (HappyWrap50 happy_var_2) -> 
	happyIn50
		 (AbsTinyCamiot.EApp () happy_var_1 happy_var_2
	)}}

happyReduce_68 = happySpecReduce_3  16# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	case happyOut50 happy_x_3 of { (HappyWrap50 happy_var_3) -> 
	happyIn50
		 (AbsTinyCamiot.EOr () happy_var_1 happy_var_3
	)}}

happyReduce_69 = happySpecReduce_1  16# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	happyIn50
		 (happy_var_1
	)}

happyReduce_70 = happySpecReduce_3  17# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	case happyOut51 happy_x_3 of { (HappyWrap51 happy_var_3) -> 
	happyIn51
		 (AbsTinyCamiot.EAnd () happy_var_1 happy_var_3
	)}}

happyReduce_71 = happySpecReduce_1  17# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	happyIn51
		 (happy_var_1
	)}

happyReduce_72 = happySpecReduce_3  18# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	case happyOut59 happy_x_2 of { (HappyWrap59 happy_var_2) -> 
	case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	happyIn52
		 (AbsTinyCamiot.ERel () happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_73 = happySpecReduce_1  18# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	happyIn52
		 (happy_var_1
	)}

happyReduce_74 = happySpecReduce_3  19# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	case happyOut54 happy_x_3 of { (HappyWrap54 happy_var_3) -> 
	happyIn53
		 (AbsTinyCamiot.EAdd () happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_75 = happySpecReduce_1  19# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	happyIn53
		 (happy_var_1
	)}

happyReduce_76 = happySpecReduce_3  20# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut58 happy_x_2 of { (HappyWrap58 happy_var_2) -> 
	case happyOut55 happy_x_3 of { (HappyWrap55 happy_var_3) -> 
	happyIn54
		 (AbsTinyCamiot.EMul () happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_77 = happySpecReduce_1  20# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	happyIn54
		 (happy_var_1
	)}

happyReduce_78 = happySpecReduce_2  21# happyReduction_78
happyReduction_78 happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	happyIn55
		 (AbsTinyCamiot.ENot () happy_var_2
	)}

happyReduce_79 = happySpecReduce_1  21# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOut56 happy_x_1 of { (HappyWrap56 happy_var_1) -> 
	happyIn55
		 (happy_var_1
	)}

happyReduce_80 = happySpecReduce_1  22# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn56
		 (AbsTinyCamiot.EVar () happy_var_1
	)}

happyReduce_81 = happySpecReduce_1  22# happyReduction_81
happyReduction_81 happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	happyIn56
		 (AbsTinyCamiot.EConst () happy_var_1
	)}

happyReduce_82 = happySpecReduce_3  22# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_2 of { (HappyWrap49 happy_var_2) -> 
	happyIn56
		 (happy_var_2
	)}

happyReduce_83 = happySpecReduce_1  23# happyReduction_83
happyReduction_83 happy_x_1
	 =  happyIn57
		 (AbsTinyCamiot.Plus ()
	)

happyReduce_84 = happySpecReduce_1  23# happyReduction_84
happyReduction_84 happy_x_1
	 =  happyIn57
		 (AbsTinyCamiot.Minus ()
	)

happyReduce_85 = happySpecReduce_1  24# happyReduction_85
happyReduction_85 happy_x_1
	 =  happyIn58
		 (AbsTinyCamiot.Times ()
	)

happyReduce_86 = happySpecReduce_1  24# happyReduction_86
happyReduction_86 happy_x_1
	 =  happyIn58
		 (AbsTinyCamiot.Div ()
	)

happyReduce_87 = happySpecReduce_1  25# happyReduction_87
happyReduction_87 happy_x_1
	 =  happyIn59
		 (AbsTinyCamiot.LTC ()
	)

happyReduce_88 = happySpecReduce_1  25# happyReduction_88
happyReduction_88 happy_x_1
	 =  happyIn59
		 (AbsTinyCamiot.LEC ()
	)

happyReduce_89 = happySpecReduce_1  25# happyReduction_89
happyReduction_89 happy_x_1
	 =  happyIn59
		 (AbsTinyCamiot.GTC ()
	)

happyReduce_90 = happySpecReduce_1  25# happyReduction_90
happyReduction_90 happy_x_1
	 =  happyIn59
		 (AbsTinyCamiot.GEC ()
	)

happyReduce_91 = happySpecReduce_1  25# happyReduction_91
happyReduction_91 happy_x_1
	 =  happyIn59
		 (AbsTinyCamiot.EQC ()
	)

happyReduce_92 = happySpecReduce_0  26# happyReduction_92
happyReduction_92  =  happyIn60
		 ([]
	)

happyReduce_93 = happySpecReduce_2  26# happyReduction_93
happyReduction_93 happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut60 happy_x_2 of { (HappyWrap60 happy_var_2) -> 
	happyIn60
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_94 = happySpecReduce_1  27# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	happyIn61
		 (AbsTinyCamiot.Constructor () happy_var_1
	)}

happyReduce_95 = happySpecReduce_1  28# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut35 happy_x_1 of { (HappyWrap35 happy_var_1) -> 
	happyIn62
		 (AbsTinyCamiot.CInt () happy_var_1
	)}

happyReduce_96 = happySpecReduce_1  28# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	happyIn62
		 (AbsTinyCamiot.CFloat () happy_var_1
	)}

happyReduce_97 = happySpecReduce_1  28# happyReduction_97
happyReduction_97 happy_x_1
	 =  happyIn62
		 (AbsTinyCamiot.CTrue ()
	)

happyReduce_98 = happySpecReduce_1  28# happyReduction_98
happyReduction_98 happy_x_1
	 =  happyIn62
		 (AbsTinyCamiot.CFalse ()
	)

happyReduce_99 = happySpecReduce_1  28# happyReduction_99
happyReduction_99 happy_x_1
	 =  happyIn62
		 (AbsTinyCamiot.CNil ()
	)

happyReduce_100 = happySpecReduce_1  29# happyReduction_100
happyReduction_100 happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	happyIn63
		 (AbsTinyCamiot.PConst () happy_var_1
	)}

happyReduce_101 = happySpecReduce_1  29# happyReduction_101
happyReduction_101 happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn63
		 (AbsTinyCamiot.PVar () happy_var_1
	)}

happyReduce_102 = happySpecReduce_2  29# happyReduction_102
happyReduction_102 happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	case happyOut66 happy_x_2 of { (HappyWrap66 happy_var_2) -> 
	happyIn63
		 (AbsTinyCamiot.PAdt () happy_var_1 happy_var_2
	)}}

happyReduce_103 = happySpecReduce_1  29# happyReduction_103
happyReduction_103 happy_x_1
	 =  happyIn63
		 (AbsTinyCamiot.PWild ()
	)

happyReduce_104 = happySpecReduce_2  29# happyReduction_104
happyReduction_104 happy_x_2
	happy_x_1
	 =  happyIn63
		 (AbsTinyCamiot.PNIl ()
	)

happyReduce_105 = happySpecReduce_1  29# happyReduction_105
happyReduction_105 happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	happyIn63
		 (happy_var_1
	)}

happyReduce_106 = happyReduce 5# 30# happyReduction_106
happyReduction_106 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut63 happy_x_2 of { (HappyWrap63 happy_var_2) -> 
	case happyOut63 happy_x_4 of { (HappyWrap63 happy_var_4) -> 
	happyIn64
		 (AbsTinyCamiot.PTup () happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_107 = happySpecReduce_1  30# happyReduction_107
happyReduction_107 happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	happyIn64
		 (happy_var_1
	)}

happyReduce_108 = happySpecReduce_3  31# happyReduction_108
happyReduction_108 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	case happyOut63 happy_x_3 of { (HappyWrap63 happy_var_3) -> 
	happyIn65
		 (AbsTinyCamiot.PLay () happy_var_1 happy_var_3
	)}}

happyReduce_109 = happySpecReduce_3  31# happyReduction_109
happyReduction_109 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut63 happy_x_2 of { (HappyWrap63 happy_var_2) -> 
	happyIn65
		 (happy_var_2
	)}

happyReduce_110 = happySpecReduce_0  32# happyReduction_110
happyReduction_110  =  happyIn66
		 ([]
	)

happyReduce_111 = happySpecReduce_2  32# happyReduction_111
happyReduction_111 happy_x_2
	happy_x_1
	 =  case happyOut63 happy_x_1 of { (HappyWrap63 happy_var_1) -> 
	case happyOut66 happy_x_2 of { (HappyWrap66 happy_var_2) -> 
	happyIn66
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_112 = happySpecReduce_3  33# happyReduction_112
happyReduction_112 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut63 happy_x_1 of { (HappyWrap63 happy_var_1) -> 
	case happyOut49 happy_x_3 of { (HappyWrap49 happy_var_3) -> 
	happyIn67
		 (AbsTinyCamiot.PM () happy_var_1 happy_var_3
	)}}

happyReduce_113 = happySpecReduce_0  34# happyReduction_113
happyReduction_113  =  happyIn68
		 ([]
	)

happyReduce_114 = happySpecReduce_1  34# happyReduction_114
happyReduction_114 happy_x_1
	 =  case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	happyIn68
		 ((:[]) happy_var_1
	)}

happyReduce_115 = happySpecReduce_3  34# happyReduction_115
happyReduction_115 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	case happyOut68 happy_x_3 of { (HappyWrap68 happy_var_3) -> 
	happyIn68
		 ((:) happy_var_1 happy_var_3
	)}}

happyNewToken action sts stk [] =
	happyDoAction 42# notHappyAtAll action sts stk []

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
	PT _ (TV happy_dollar_dollar) -> cont 38#;
	PT _ (TI happy_dollar_dollar) -> cont 39#;
	PT _ (TD happy_dollar_dollar) -> cont 40#;
	PT _ (T_UIdent happy_dollar_dollar) -> cont 41#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 42# tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap38 x') = happyOut38 x} in x'))

pDef tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap39 x') = happyOut39 x} in x'))

pConstructorDec tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap40 x') = happyOut40 x} in x'))

pListConstructorDec tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap41 x') = happyOut41 x} in x'))

pListIdent tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap42 x') = happyOut42 x} in x'))

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (let {(HappyWrap43 x') = happyOut43 x} in x'))

pType1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (let {(HappyWrap44 x') = happyOut44 x} in x'))

pType2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (let {(HappyWrap45 x') = happyOut45 x} in x'))

pListType tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (let {(HappyWrap46 x') = happyOut46 x} in x'))

pTupExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (let {(HappyWrap47 x') = happyOut47 x} in x'))

pListTupExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (let {(HappyWrap48 x') = happyOut48 x} in x'))

pExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (let {(HappyWrap49 x') = happyOut49 x} in x'))

pExp1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (let {(HappyWrap50 x') = happyOut50 x} in x'))

pExp2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (let {(HappyWrap51 x') = happyOut51 x} in x'))

pExp3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (let {(HappyWrap52 x') = happyOut52 x} in x'))

pExp4 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (let {(HappyWrap53 x') = happyOut53 x} in x'))

pExp5 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (let {(HappyWrap54 x') = happyOut54 x} in x'))

pExp6 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (let {(HappyWrap55 x') = happyOut55 x} in x'))

pExp7 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (let {(HappyWrap56 x') = happyOut56 x} in x'))

pAddOp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (let {(HappyWrap57 x') = happyOut57 x} in x'))

pMulOp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (let {(HappyWrap58 x') = happyOut58 x} in x'))

pRelOp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (let {(HappyWrap59 x') = happyOut59 x} in x'))

pListExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (let {(HappyWrap60 x') = happyOut60 x} in x'))

pCon tks = happySomeParser where
 happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (let {(HappyWrap61 x') = happyOut61 x} in x'))

pConst tks = happySomeParser where
 happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (let {(HappyWrap62 x') = happyOut62 x} in x'))

pPat tks = happySomeParser where
 happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (let {(HappyWrap63 x') = happyOut63 x} in x'))

pPat1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (let {(HappyWrap64 x') = happyOut64 x} in x'))

pPat2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 27# tks) (\x -> happyReturn (let {(HappyWrap65 x') = happyOut65 x} in x'))

pListPat tks = happySomeParser where
 happySomeParser = happyThen (happyParse 28# tks) (\x -> happyReturn (let {(HappyWrap66 x') = happyOut66 x} in x'))

pPatMatch tks = happySomeParser where
 happySomeParser = happyThen (happyParse 29# tks) (\x -> happyReturn (let {(HappyWrap67 x') = happyOut67 x} in x'))

pListPatMatch tks = happySomeParser where
 happySomeParser = happyThen (happyParse 30# tks) (\x -> happyReturn (let {(HappyWrap68 x') = happyOut68 x} in x'))

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
