# SEO check

A seo checker for CI.

This is designed to be run on CI to make sure that you do not have bad SEO manners before you deploy

## Using seocheck

Start your server locally on `localhost:8000` and run `seocheck` as follows:

```
seocheck http://localhost:8000
```

### Using seocheck as part of a nix derivation

```
stdenv.mkDerivation {
  name = "site";
  buildInputs = [ final.haskellPackages.seocheck final.killall ];
  buildCommand = ''
    mkdir -p $out
    cp -r ${site}/. $out

    $out/bin/site &
    seocheck http://localhost:8000
    killall site
  '';
};
```

## Limitations

* `seocheck` does not try to run JavaScript
* Does not try to fetch external links
