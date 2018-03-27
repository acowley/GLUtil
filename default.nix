{ mkDerivation, array, base, bytestring, containers, directory
, filepath, hpp, JuicyPixels, linear, OpenGL, OpenGLRaw, stdenv
, transformers, vector
}:
mkDerivation {
  pname = "GLUtil";
  version = "0.10.1";
  src = ./.;
  libraryHaskellDepends = [
    array base bytestring containers directory filepath hpp JuicyPixels
    linear OpenGL OpenGLRaw transformers vector
  ];
  description = "Miscellaneous OpenGL utilities";
  license = stdenv.lib.licenses.bsd3;
}
