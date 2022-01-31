let upstream = https://github.com/dfinity/vessel-package-set/releases/download/mo-0.6.20-20220131/package-set.dhall sha256:5d3cc1e3be83d178f0e2210998f8266e536ebd241f415df8e87feb53effe6254 

let Package =
    { name : Text, version : Text, repo : Text, dependencies : List Text }

let
  -- This is where you can add your own packages to the package-set
  additions = [] : List Package

let
  -- This is where you can override existing packages in the package-set
  overrides = [] : List Package

-- in  upstream # additions # overrides
in upstream # additions # overrides
