data Wu = Extend (Float -> Float) | Yet Float

{- Examples    -}

a1 = Extend (*10)
a2 = Yet 5
a3 = Extend (\x -> 5)
a4 = Extend (+1)
m = Extend (10**)
s = Extend (0.1**)
c = Extend id
c2 = log 2 / log m    {- log(M,2) 以二為底，M的對數。也就是二進制時要表現出0~1所有數所需要的位數-}
c3 = log 3 / log m
c4 = log 4 / log m    {- log(M,4) 以四為底，M的對數。也就是四進制時要表現出0~1所有數所需要的位數，比二進制少一半-}
c10 = log 10 / log m



main = do
    print (c10 - c)	{-	0	-}
    print ((1/m) * m)   		{-	1	-}	
    print (c2 / c4)				{-	2	-}	
    print (10**c - m + 3)		{-	3	-}





{- Structures  -}

list :: Wu -> [Float]
list (Extend f) = map f [0..5]
list (Yet x) = [x | _ <- [0..5]]

long :: Wu -> [Float]
long (Extend f) = map f [0..100]
long (Yet x) = [x | _ <- [0..100]]

instance Show Wu where
	show (Yet c) = show c
	show x = show (list x)

instance Eq Wu where
	(==) x y = list x == long y {- not strict  -}

instance Ord Wu where
	compare x y = compare (last $ long x) (last $ long y) {- not strict  -}

negateWu :: Wu -> Wu
negateWu (Extend f) = Extend (\x -> 0 - f x)
negateWu (Yet c) = Yet (-c)

instance Num Wu where
	(Extend f) + (Extend g) = Extend (\x -> f x + g x)
	(Extend f) + (Yet c) = Extend (\x -> f x + c)
	(Yet c) + (Extend f) = Extend (\x -> f x + c)
	(Yet k) + (Yet c) = Yet (k+c)
	x - y = x + negateWu y
	(Extend f) * (Extend g) = Extend (\x -> f x * g x)
	(Extend f) * (Yet c) = Extend (\x -> f x * c)
	(Yet c) * (Extend f) = Extend (\x -> f x * c)
	(Yet k) * (Yet c) = Yet (k*c)
	abs (Extend f) = Extend (f.abs)
	abs (Yet c) = Yet (abs c)
	signum x = Yet (signum (last $ list x))
	fromInteger c = Yet (fromInteger c)

instance Fractional Wu where
	(Extend f) / (Extend g) = Extend (\x -> f x / g x)
	(Extend f) / (Yet c) = Extend (\x -> f x / c)
	(Yet c) / (Extend f) = Extend (\x -> f x / c)
	(Yet k) / (Yet c) = Yet (k/c)
	fromRational c = Yet (fromRational c)




instance Floating Wu where
	pi = Yet pi
	exp (Extend f) = Extend (exp.f)
	exp (Yet c) = Yet (exp c)
	log (Extend f) = Extend (log.f)
	log (Yet c) = Yet (log c)
	logBase x y = log y / log x
	sin (Extend f) = Extend (sin.f)
	sin (Yet c) = Yet (sin c)
	cos (Extend f) = Extend (cos.f)
	cos (Yet c) = Yet (cos c)
	tan (Extend f) = Extend (tan.f)
	tan (Yet c) = Yet (tan c)
	sinh (Extend f) = Extend (sinh.f)
	sinh (Yet c) = Yet (sinh c)
	cosh (Extend f) = Extend (cosh.f)
	cosh (Yet c) = Yet (cosh c)
	tanh (Extend f) = Extend (tanh.f)
	tanh (Yet c) = Yet (tanh c)
	asin (Extend f) = Extend (asin.f)
	asin (Yet c) = Yet (asin c)
	acos (Extend f) = Extend (acos.f)
	acos (Yet c) = Yet (acos c)
	atan (Extend f) = Extend (atan.f)
	atan (Yet c) = Yet (atan c)
	asinh (Extend f) = Extend (asinh.f)
	asinh (Yet c) = Yet (asinh c)
	acosh (Extend f) = Extend (acosh.f)
	acosh (Yet c) = Yet (acosh c)
	atanh (Extend f) = Extend (atanh.f)
	atanh (Yet c) = Yet (atanh c)
	(Extend f) ** (Extend g) = Extend (\x -> f x ** g x)
	(Extend f) ** (Yet c) = Extend (\x -> f x ** c)
	(Yet c) ** (Extend f) = Extend (\x -> f x ** c)
	(Yet k) ** (Yet c) = Yet (k**c)


{- 應用 -}



{- 三論康托集 -}

cantor1 = 2 ** c3
cantor2 = m ** (log 3 / log 2) {- -}
cantor3 = m * ((2/3) ** c3)   {-從[0,1)線段的點測度出發-}

{-
	cantor1 = 2 ** c3
			= 2 ** (log 3 / log m)
			= 3 ** (logBase 3 2) **logBase 3 m
			= 3 ** logBase 3 m ** (logBase 3 2)
			= m ** (logBase 3 2)
			= m ** (log 3 / log 2)
			= cantor2

	cantor3 = m * ((2/3) ** c3)
			= m	* 2**(log 3 / log m) / 3 ** (log 3 / log m)
			= m	* 2**(log 3 / log m) / 3 ** (logBase 3 m)
			= m * 2**(log 3 / log m) / m
			= 2**(log 3 / log m)
			= 2 ** c3
			= cantor1

	thus, cantor1 = cantor2 = cantor3

-}

