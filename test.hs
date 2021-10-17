a = "brah"

data D = DC1 Int | DC2 Int Int

-- x = DC1 2;
-- x = DC2 1;
-- x = DC2 2 (2 + 1)
-- x = DC1 (4/2)

f :: [Maybe a] -> t
f (Nothing:xs) = f xs

func = let
        f = flip filter $ [1..]
        in (take 3 $ (f even)) ++ (take 3 $ (f odd))
        