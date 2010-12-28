ghc -e main \
  -isrc \
  -package ghc \
  -cpp \
  -XGeneralizedNewtypeDeriving \
  -XMultiParamTypeClasses \
  -XDeriveDataTypeable \
  -XMagicHash \
  -XTypeSynonymInstances \
  -XFlexibleInstances \
  -XFlexibleContexts \
  -XFunctionalDependencies \
  -XKindSignatures \
  -XRank2Types \
  -XScopedTypeVariables \
  -XExistentialQuantification \
  -XPatternGuards \
   unit-tests/run-unit-tests.hs
