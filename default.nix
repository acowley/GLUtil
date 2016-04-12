{ mkDerivation, array, base, bytestring, containers, hpp
, directory, filepath, JuicyPixels, linear, OpenGL, OpenGLRaw
, stdenv, transformers, vector
}:
mkDerivation {
  pname = "GLUtil";
  version = "0.9.0";
  src = ./.;
  libraryHaskellDepends = [
    array base bytestring containers directory filepath JuicyPixels
    linear OpenGL OpenGLRaw transformers vector hpp
  ];
  libraryToolDepends = [ hpp ];
  description = "Miscellaneous OpenGL utilities";
  license = stdenv.lib.licenses.bsd3;
}
