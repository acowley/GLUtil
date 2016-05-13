{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-5_11" }:

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
                       else if compiler == "mine"
                            then pkgs.myHaskellPackages.override {
                              overrides = self: super: {
                                OpenGLRaw = self.callPackage ./OpenGLRaw-3.2.0.0 {};
                                GLURaw = self.callPackage ./GLURaw-2.0.0.2 {};
                                OpenGL = self.callPackage ./OpenGL-3.0.1.0 {};
                              };
                            }
                            else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
