options = ["--ghc-options=-ignore-dot-ghci -DTEST"]

notification :growl, sticky: false
guard :haskell, repl_options: options do
  watch(%r{src/.+\.l?hs$})
  watch(%r{test/.+Spec\.l?hs$})
  watch(%r{\.cabal$})
  notification :growl
end
