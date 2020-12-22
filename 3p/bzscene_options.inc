(*====< BZScene_Options.inc >==================================================
  ------------------------------------------------------------------------------
  Historique :
   - 18/11/16 - BeanzMaster - Creation
  ------------------------------------------------------------------------------
  Description :
  - Définie les paramètres de compilation globales pour la bibliothèque
  
  #ENGLISH :
  - Define global compilation parameters for the library
================================================================================*)

//==============================================================================
//----[ General Options ]-------------------------------------------------------
//==============================================================================

{.$DEFINE DEBUG}
{.$DEFINE DEBUGLOG}

  
  {$IFDEF CPUI386}
    // On utilise le jeu d'instruction 32bit
    {$DEFINE CPU32}
    {$ASMMODE INTEL}
  {$ELSE}
    {$IFDEF CPUX86_64}
      // On utilise le jeu d'instruction 64bit
      {$DEFINE CPU64}
      {$ASMMODE INTEL}
    {$ENDIF}
  {$ENDIF}

  {.$LONGSTRINGS ON}

  {$IFDEF RELEASE}
    {$INLINE ON}
    {$DEFINE USE_ASM_OPTIMIZATIONS}
    {$DEFINE USE_FASTMATH}

  {$ENDIF}

  {$IFNDEF WINDOWS}
    {$LINKLIB c}
  {$ENDIF}

//==============================================================================
//----[ Mode Debug ]------------------------------------------------------------
// Enable / disable debug mode
//==============================================================================

{$IFDEF DEBUG}
  // Activer tous les avertissements 'Unsafe Type' / 'Unsafe Code'
  {$DEFINE DEBUG_ENABLE_UNSAFE_WARNINGS}

  {$WARN 5036 on : Local variable "$1" does not seem to be initialized}
  {$WARN 5025 on : Local variable "$1" not used}
  {$WARN 5028 on : Local const "$1" not used}

  {$DEFINE USE_FASTMATH}
  // Enable / disable OpenGL Debugging (Only with OpenGL 4.0 and up)
  //{$DEFINE GLS_OPENGL_DEBUG}
{$ENDIF}

{$IFNDEF DEBUG}
  {$WARN 5036 off : Local variable "$1" does not seem to be initialized}
  {$WARN 5025 off : Local variable "$1" not used}
  {$WARN 5028 off : Local const "$1" not used}
  {$WARN 5057 off : Note: Local variable "$1" does not seem to be initialized }
  {$WARN 5028 off : Note: Local type "$1" not used }
  {$WARN 5024 off : Parameter "$1" not used}
  {$WARN 2005 off : Comment level $1 found}
  {.$WARN 5029 off : Note: Private field "$1" is never used }
  {.$WARN 5071 off : Note: Private type "$1" never used }
{$ENDIF}

//==============================================================================
//----[Extra parameters ]-------------------------------------------------------
//==============================================================================
// Code Alignment
{$IFDEF CPU64}
  {$ALIGN 16}
  {$CODEALIGN CONSTMIN=16}
  {$CODEALIGN LOCALMIN=16}
  {$CODEALIGN VARMIN=16}
  // Enable / disable Assembly optimizations
  {$IFDEF WINDOWS}
    {$DEFINE USE_ASM_OPTIMIZATIONS}
  {$ENDIF}
{$ENDIF}



{$IFDEF CPU64}
  {$IFDEF USE_ASM_OPTIMIZATIONS}
    {.$DEFINE USE_ASM_SSE_3}
    {.$DEFINE USE_ASM_AVX}
    {.$DEFINE USE_ASM_SSE_4}
  {$ENDIF}
{$ELSE}
  {$IFDEF USE_ASM_OPTIMIZATIONS}
    {$DEFINE USE_ASM_SSE_3}
    {.$DEFINE USE_ASM_AVX}
    {.$DEFINE USE_ASM_SSE_4}
  {$ENDIF}
{$ENDIF}


{$IFDEF USE_ASM_AVX}
   {$DEFINE USE_ASM_SSE_4}
{$ENDIF}

{$IFDEF USE_ASM_SSE_4}
   {$DEFINE USE_ASM_SSE_3}
   {$DEFINE USE_ASM}
{$ENDIF}

{$IFDEF USE_ASM_SSE_3}
  {$DEFINE USE_ASM}
{$ENDIF}

{$DEFINE USE_ASM_SIMD_HIGHPRECISION}

{$IF fpc_fullversion > 031000}
   {$DEFINE USE_VECTORCALL}
{$ENDIF}

{$DEFINE USE_FASTMATH}

//==============================================================================
//----[ System parameters ]-----------------------------------------------------
//==============================================================================

//------------------------------------------------------------------------------
// Detect OS
//------------------------------------------------------------------------------
{$IFDEF WINDOWS}
  // Par défaut, GLX n'est pas pris en charge sur Windows, bien que les implémentations Windows existent
  {$UNDEF GLX_SUPPORT}
  {$UNDEF X11_SUPPORT}
  {$DEFINE WGL_SUPPORT}
{$ENDIF}

//------------------------------------------------------------------------------
{$IFDEF LINUX}
  {$DEFINE LCLGTK2}
  {$DEFINE UNIX}
  {$UNDEF WGL_SUPPORT}
  //Enable X Window support
  {$DEFINE X11_SUPPORT}
  {$DEFINE GLX_SUPPORT}
  {$DEFINE UseCThread}
{$ENDIF}

//------------------------------------------------------------------------------
{$IFDEF DARWIN}
  {$DEFINE NO_ASM_OPTIMIZATIONS}
  {$UNDEF WGL_SUPPORT}
  {$UNDEF GLX_SUPPORT}
  {$UNDEF X11_SUPPORT}
{$ENDIF}


//------------------------------------------------------------------------------

//==============================================================================

