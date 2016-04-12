{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-5_1" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, bytestring, containers, hpp
      , directory, filepath, JuicyPixels, linear, OpenGL, OpenGLRaw
      , stdenv, transformers, vector
      }:
      mkDerivation {
        pname = "GLUtil";
        version = "0.8.8";
        src = ./.;
        libraryHaskellDepends = [
          array base bytestring containers directory filepath JuicyPixels
          linear OpenGL OpenGLRaw transformers vector
        ];
        libraryToolDepends = [ hpp ];
        buildDepends = with pkgs;
          lib.optionals stdenv.isDarwin [darwin.apple_sdk.frameworks.OpenGL];
        description = "Miscellaneous OpenGL utilities";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
