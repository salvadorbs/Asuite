param ([string] $ASuiteSource, [string] $LazBuildPath, [switch] $BuildIde = $false, [switch] $RemoveSparta = $false)

# *** FUNCTION DEFINITIONS

function InstallPackage($LazBuildPath, $PackagePath)
{
  write-output "Installing Package $PackagePath"
  $LazBuild_AllArgs = @('--add-package', "$PackagePath", '--skip-dependencies', '-q')
  & $LazBuildPath $LazBuild_AllArgs
}

function InstallPackageLink($LazBuildPath, $PackagePath)
{
  write-output "Installing Package (only link) $PackagePath"
  $LazBuild_AllArgs = @('--add-package-link', "$PackagePath", '--skip-dependencies', '-q')
  & $LazBuildPath $LazBuild_AllArgs
}

function BuildIde($LazBuildPath)
{
  write-output "Building Lazarus IDE"
  $LazBuild_AllArgs = @('--build-ide=')
  & $LazBuildPath $LazBuild_AllArgs
}

# *** BEGIN MAIN SCRIPT

write-output "ASuiteSource = $ASuiteSource"
write-output "LazBuildPath = $LazBuildPath"
write-output "BuildIde = $BuildIde"
write-output "RemoveSparta = $RemoveSparta"

# Remove Sparta_Generics from jpLib.lpk requirements
if ($RemoveSparta) {
    write-output "Removing Sparta_Generics from jpLib.lpk"

    $file = Get-Item $ASuiteSource/3p/JPLib/packages/Lazarus/jplib.lpk
    [xml]$xml = Get-Content -Path $file
    $node = $xml.SelectSingleNode('CONFIG/Package/RequiredPkgs/Item1')
    $node.ParentNode.RemoveChild($node)
    $xml.Save($file.Fullname)
}

# Install Lazarus Components
write-output "Installing Lazarus Components for building ASuite"
InstallPackageLink $LazBuildPath "$ASuiteSource/3p/bgrabitmap/bgrabitmap/bgrabitmappack.lpk"
InstallPackage $LazBuildPath "$ASuiteSource/3p/bgrabitmap/bglcontrols/bglcontrols.lpk"
InstallPackage $LazBuildPath "$ASuiteSource/3p/bgracontrols/bgracontrols.lpk"
InstallPackage $LazBuildPath "$ASuiteSource/3p/bgracontrols/bgrapascalscriptcomponent.lpk"
InstallPackage $LazBuildPath "$ASuiteSource/3p/IGDIPlusMod/packages/Lazarus/lazigdiplus.lpk"
InstallPackage $LazBuildPath "$ASuiteSource/3p/JPLib/packages/Lazarus/jplib.lpk"
InstallPackage $LazBuildPath "$ASuiteSource/3p/JPPack/packages/Lazarus/jppacklcl.lpk"
InstallPackageLink $LazBuildPath "$ASuiteSource/3p/mORMot2/packages/lazarus/mormot2.lpk"
InstallPackageLink $LazBuildPath "$ASuiteSource/3p/HashLib4Pascal/HashLib/src/Packages/FPC/HashLib4PascalPackage.lpk"
InstallPackage $LazBuildPath "$ASuiteSource/3p/luipack/uniqueinstance/uniqueinstance_package.lpk"
InstallPackageLink $LazBuildPath "$ASuiteSource/3p/luipack/lclextensions/lclextensions_package.lpk"
InstallPackage $LazBuildPath "$ASuiteSource/3p/VirtualTreeView-Lazarus/Source/virtualtreeview_package.lpk"
InstallPackage $LazBuildPath "$ASuiteSource/3p/AsuiteComps/ASuiteComps.lpk"

# Download mORMot 2 Static files and extract them in proper directory
$Url = 'https://github.com/synopse/mORMot2/releases/download/2.0.3306/mormot2static.7z' 
$ZipFile = $(Split-Path -Path $Url -Leaf) 
$Destination = $ASuiteSource + '/3p/mORMot2/static/'
$pwd = Get-Location

write-output "Downloading $ZipFile for mORMot2"
Invoke-WebRequest -Uri $Url -OutFile $ZipFile

$command = "7z x '$ZipFile' -o'$Destination'"
Invoke-Expression $command

if ($BuildIde) {
  BuildIde $LazBuildPath
}