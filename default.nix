{ mkDerivation, array, base, binary, bytestring, containers
, directory, filepath, GLFW-b, hpp, JuicyPixels, linear, OpenGL
, OpenGLRaw, stdenv, transformers, vector
}:
mkDerivation {
  pname = "GLUtil";
  version = "0.10.4";
  src = ./.;
  configureFlags = [ "-fdemos" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bytestring containers directory filepath JuicyPixels
    linear OpenGL OpenGLRaw transformers vector
  ];
  libraryToolDepends = [ hpp ];
  executableHaskellDepends = [
    base binary bytestring filepath GLFW-b OpenGL
  ];
  description = "Miscellaneous OpenGL utilities";
  license = stdenv.lib.licenses.bsd3;
}
