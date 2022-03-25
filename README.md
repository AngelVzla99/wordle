# Wordle


## Consideraciones!

### Sobre la suite de testeo.

La suite de testeo asume que todos los modulos exportan absolutamente todas sus funciones, asi que antes de correrla
por favor comentar los exports de cada modulo. Creo que la solucion a esto seria separar el modulo en dos: internals (que exportan todo) y externals (que exporta solo lo que el usuario usa), asi la suite puede testear todo lo que quiera.

### Problemas en windows

El juego NO va funcionar correctamente en windows debido a un problema con la funcion `getChar`, si se desea que corra en windows, por favor modificar la funcion `getChar'` a lo siguiente: 

```haskell
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

-- Multi-platform version of `getChar` which has a fix for a GHC bug with Windows cmd/Powershell
getChar' :: IO Char
getChar' = withNoBuffering $ do
#ifdef mingw32_HOST_OS
      -- Windows has to do things...
      c <- c_getch
      let c' = chr . fromEnum $ c
      return c'
#else
    -- Linux, Unix, Mac OS X can just use the normal getChar
    getChar
#endif

#ifdef mingw32_HOST_OS  
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
#endif
```

(basicamente usar el `getch` de C++).

Otra consideracion es que powershell/cmd NO pega bien los caracteres unicode, por lo tanto el solver NO correra correctamente, (aun falta testear en emuladores de terminal como cygwig).

Un workaround que se puede tomar es tecleando A MANO el input (en vez de pegarlo).

