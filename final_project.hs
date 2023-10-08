-- Interpretation of Mandarin Negation Construction "差点儿(没/没有)"
-- by Jing Ji

-- syntax

data S = S NP VP'
        | SN NP NGVP
        | SM NP MVP
         deriving Show
         
data NP = NP N
        | PRN String Int
         deriving Show

data VP = IVP IV
          | TVP TV NP
         deriving Show
         
data VP' = IVP' IV AU
          | TVP' TV AU NP
         deriving Show
         
data NGVP = NGVP NG VP
         deriving Show
         
data MVP = MVP MD VP 
          | MVP' MD VP'
          | MNG MD NGVP
          | MNG' MD NG VP'
         deriving Show

data N = N String
         deriving Show
         
data IV = IV String
         deriving Show

data TV = TV String
         deriving Show
         
data AU = AU String -- auxiliary word "了"
          deriving Show

data NG = NG String -- negative adverb "没/没有"
          deriving Show

data MD = MD String -- modal adverb with implicit negation "差点儿"
          deriving Show

data Sentiment = Positive | Negative
          deriving (Show,Eq)

s1,s2,s3,s4,s5,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24 :: S
s1 = SM (NP (N "Michael")) (MVP' (MD "差点儿") (IVP' (IV "结婚") (AU "了")))
s2 = SN (NP (N "Michael")) (NGVP (NG "没") (IVP (IV "结婚")))
s3 = S (NP (N "Michael")) (IVP' (IV "结婚") (AU "了"))
s4 = SM (NP (N "Michael")) (MVP (MD "差点儿") (IVP (IV "结婚")))
s5 = SM (PRN "他" 1) (MNG (MD "差点儿") (NGVP (NG "没") (IVP (IV "结婚"))))

s7 = SM (NP (N "Yang")) (MVP' (MD "差点儿") (IVP' (IV "摔倒") (AU "了")))
s8 = SN (NP (N "Yang")) (NGVP (NG "没") (IVP (IV "摔倒")))
s9 = S (NP (N "Yang")) (IVP' (IV "摔倒") (AU "了"))
s10 = SM (NP (N "Yang")) (MVP (MD "差点儿") (IVP (IV "摔倒")))
s11 = SM (PRN "她" 2) (MNG (MD "差点儿") (NGVP (NG "没") (IVP (IV "摔倒"))))
s12 = SM (NP (N "Yang")) (MNG' (MD "差点儿") (NG "没") (IVP' (IV "摔倒") (AU "了")))

s13 = SM (NP (N "Ayla")) (MVP' (MD "差点儿") (TVP' (TV "撞到") (AU "了") (NP (N "Suji"))))
s14 = SN (NP (N "Ayla")) (NGVP (NG "没") (TVP (TV "撞到") (NP (N "Suji"))))
s15 = S (NP (N "Ayla")) (TVP' (TV "撞到") (AU "了") (NP (N "Suji")))
s16 = SM (NP (N "Ayla")) (MVP (MD "差点儿") (TVP (TV "撞到") (NP (N "Suji"))))
s17 = SM (PRN "她" 3) (MNG (MD "差点儿") (NGVP (NG "没") (TVP (TV "撞到") (NP (N "Suji")))))
s18 = SM (NP (N "Ayla")) (MNG' (MD "差点儿") (NG "没") (TVP' (TV "撞到") (AU "了") (NP (N "Suji"))))

s19 = SM (PRN "我" 4) (MVP' (MD "差点儿") (TVP' (TV "得") (AU "了") (NP (N "60分"))))
s20 = SN (PRN "我" 4) (NGVP (NG "没") (TVP (TV "得") (NP (N "80分"))))
s21 = SM (PRN "我" 4) (MVP' (MD "差点儿") (TVP' (TV "得") (AU "了") (NP (N "100分"))))
s22 = SM (PRN "我" 4) (MVP' (MD "差点儿") (TVP' (TV "得") (AU "了") (NP (N "80分"))))
s23 = S (PRN "我" 4) (TVP' (TV "得") (AU "了") (NP (N "100分")))
s24 = SM (PRN "我" 4) (MNG' (MD "差点儿") (NG "没") (TVP' (TV "得") (AU "了") (NP (N "60分"))))

-- model

data Entity = M | NZ | Y | A | SJ | J | N1 | N2 | N3 deriving (Show,Eq)

nazila, michael, yang, ayla, suji, jing :: Entity
nazila = NZ
michael = M
yang = Y
ayla = A
suji = SJ
jing = J
point60 = N1
point80 = N2
point100 = N3

marry, fall_down :: Entity -> Bool
marry = \x -> elem x [NZ]
fall_down = \x -> elem x [M]

run_into, obtain :: Entity -> Entity -> Bool
run_into = \y -> \x -> elem (x,y) []
obtain = \y -> \x -> elem (x,y) [(J,N2)]

-- variable assignment

g :: Int -> Entity
g 1 = M
g 2 = Y
g 3 = A
g 4 = J

-- sentiment evaluation

sentimentVP :: VP -> (Int -> Entity) -> Sentiment
sentimentVP (IVP (IV "结婚")) g = Positive
sentimentVP (IVP (IV "摔倒")) g = Negative
sentimentVP (TVP (TV "撞到") _) g = Negative
sentimentVP (TVP (TV "得") (NP (N "60分"))) g = Negative
sentimentVP (TVP (TV "得") (NP (N "100分"))) g = Positive
sentimentVP (TVP (TV "得") (NP (N "80分"))) g = Negative

sentimentVP' :: VP' -> (Int -> Entity) -> Sentiment
sentimentVP' (IVP' iv au) g = sentimentVP (IVP iv) g
sentimentVP' (TVP' tv au np) g = sentimentVP (TVP tv np) g

-- probability evaluation

probabilityS :: S -> (Int -> Entity) -> Int
probabilityS (S (NP (N "Michael")) (IVP' (IV "结婚") (AU "了"))) g = 90
probabilityS (S (NP (N "Yang")) (IVP' (IV "摔倒") (AU "了"))) g = 95 
probabilityS (S (PRN "她" 2) (IVP' (IV "摔倒") (AU "了"))) g = 95 
probabilityS (S (PRN "她" 3) (TVP' (TV "撞到") (AU "了") (NP (N "Suji")))) g = 99

probabilityS (S (NP (N "Ayla")) (TVP' (TV "撞到") (AU "了") (NP (N "Suji")))) g = 99
probabilityS (S (PRN "我" 4) (TVP' (TV "得") (AU "了") (NP (N "60分")))) g = 98
probabilityS (S (PRN "我" 4) (TVP' (TV "得") (AU "了") (NP (N "80分")))) g = 100
probabilityS (S (PRN "我" 4) (TVP' (TV "得") (AU "了") (NP (N "100分")))) g = 95

probabilityS (SN (NP (N "Michael")) (NGVP (NG "没") (IVP (IV "结婚")))) g = 10
probabilityS (SN (PRN "我" 4) (NGVP (NG "没") (TVP (TV "得") (NP (N "100分"))))) g = 5
probabilityS (SN (PRN "他" 1) (NGVP (NG "没") (IVP (IV "结婚")))) g = 10

probabilityS (SM np (MVP' md vp')) g = probabilityS (S np vp') g
probabilityS (SM np (MVP md (IVP iv))) g = probabilityS (S np (IVP' iv (AU "了"))) g
probabilityS (SM np (MVP md (TVP tv n))) g = probabilityS (S np (TVP' tv (AU "了") n)) g
probabilityS (SM np (MNG md (NGVP ng vp))) g = if sentimentVP vp g == Negative then probabilityS (SM np (MVP md vp)) g else probabilityS (SN np (NGVP ng vp)) g
probabilityS (SM np (MNG' md ng vp')) g = probabilityS (S np vp') g 

-- interpretation of terminal nodes

intN :: N -> (Int -> Entity) -> Entity
intN (N "Michael") g = michael
intN (N "Yang") g = yang
intN (N "Ayla") g = ayla
intN (N "Suji") g = suji
intN (N "60分") g = point60
intN (N "80分") g = point80
intN (N "100分") g = point100


intIV :: IV ->  (Int -> Entity) -> (Entity -> Bool)
intIV (IV "结婚") g = marry
intIV (IV "摔倒") g = fall_down

intTV :: TV -> (Int -> Entity) -> (Entity -> Entity -> Bool)
intTV (TV "撞到") g = run_into
intTV (TV "得") g = obtain

-- interpretation of non-terminal nodes

intNP :: NP ->  (Int -> Entity) -> Entity
intNP (NP n) g = intN n g
intNP (PRN _ index) g = g index

intVP :: VP ->  (Int -> Entity) -> (Entity -> Bool)
intVP (IVP v) g = intIV v g
intVP (TVP v np) g = (intTV v g) (intNP np g)

intVP' :: VP' ->  (Int -> Entity) -> (Entity -> Bool) 
intVP' (IVP' v au) g = intIV v g
intVP' (TVP' v au np) g = (intTV v g) (intNP np g)

intNGVP :: NGVP ->  (Int -> Entity) -> (Entity -> Bool)
intNGVP (NGVP ng vp) g = not.(intVP vp g)

intMVP :: MVP -> (Int -> Entity) -> (Entity -> Bool)
intMVP (MVP' md vp') g = not.(intVP' vp' g)
intMVP (MVP md vp) g = not.(intVP vp g)
intMVP (MNG md (NGVP ng vp)) g = if sentimentVP vp g == Negative then not.(intVP vp g) else intVP vp g
intMVP (MNG' md ng vp') g = not.(intVP' vp' g)

intS :: S -> (Int -> Entity) -> Bool
intS (S np vp') g = (intVP' vp' g) (intNP np g)
intS (SN np ngvp) g = (intNGVP ngvp g) (intNP np g)
intS (SM np mvp) g = if probabilityS (SM np mvp) g >= 90 then (intMVP mvp g) (intNP np g) else False

