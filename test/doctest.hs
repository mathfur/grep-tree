import Test.DocTest

main :: IO ()
main = doctest ["-Lcabal-dev/lib", 
                "-packagebytestring-0.9.2.1",
                "-package-conf=cabal-dev/packages-7.4.1.conf", 
                "-isrc",
                "src/Main.hs"]
