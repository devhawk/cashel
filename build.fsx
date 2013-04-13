#if BOOT
open Fake
module FB = Fake.Boot
FB.Prepare {
    FB.Config.Default __SOURCE_DIRECTORY__ with
        NuGetDependencies =
            let (!!) x = FB.NuGetDependency.Create x
            [
                !!"FAKE"
                !!"NuGet.Build"
                !!"NuGet.Core"
                !!"NUnit.Runners"
            ]
}
#endif

#load ".build/boot.fsx"

open Fake 
open Fake.AssemblyInfoFile
open Fake.MSBuild

(* properties *)
let projectName = "Cashel"
let version = "1.0.0"  
let projectSummary = "A simple monadic parser combinator library."
let projectDescription = "A simple monadic parser combinator library."
let authors = ["Harry Pierson";"Ryan Riley"]
let mail = "ryan.riley@panesofglass.org"
let homepage = "https://github.com/panesofglass/cashel"

(* Directories *)
let buildDir = "./build/"
let deployDir = "./deploy/"
let testDir = "./test/"
let nugetDir = "./nuget/"
let nugetLibDir = nugetDir @@ "lib"

(* Tools *)
let nugetPath = ".nuget/NuGet.exe"
let nunitPath = "packages/NUnit.Runners.2.6.2/tools"

(* files *)
let appReferences =
    !+ @"src/Cashel/*.fsproj"
        |> Scan

let testReferences =
    !+ @"src/**/*.fsproj"
        |> Scan

let filesToZip =
    !+ (buildDir + "/**/*.*")
        -- "*.zip"
        |> Scan

(* Targets *)
Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir; deployDir; nugetDir; nugetLibDir]
)

Target "BuildApp" (fun _ -> 
    if not isLocalBuild then
        [ Attribute.Version(buildVersion)
          Attribute.Title(projectName)
          Attribute.Description(projectDescription)
          Attribute.Guid("bb0f7308-c60a-471a-84ed-a76f64469028")
        ]
        |> CreateFSharpAssemblyInfo "src/AssemblyInfo.fs"

    MSBuildRelease buildDir "Build" appReferences
        |> Log "AppBuild-Output: "
)

Target "BuildTest" (fun _ -> 
    MSBuildDebug testDir "Build" testReferences
        |> Log "TestBuild-Output: "
)

Target "Test" (fun _ ->
    let nunitOutput = testDir + "TestResults.xml"
    !+ (testDir + "Cashel.*.dll")
      |> Scan
      |> NUnit (fun p -> 
                  {p with 
                     ToolPath = nunitPath
                     DisableShadowCopy = true
                     OutputFile = nunitOutput})
)

Target "CopyLicense" (fun _ ->
    [ "LICENSE.txt" ] |> CopyTo buildDir
)

Target "BuildZip" (fun _ ->
    let zipFileName = deployDir + sprintf "%s-%s.zip" projectName version
    Zip buildDir zipFileName filesToZip
)

Target "CreateNuGet" (fun _ ->
    let nugetLibDir = nugetDir @@ "lib"
    XCopy (buildDir.TrimEnd('/')) nugetLibDir
    NuGet (fun p ->
        {p with
            Authors = authors
            Project = projectName
            Description = projectDescription
            Version = version
            OutputPath = nugetDir
            ToolPath = nugetPath
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey" })
        "Cashel.nuspec"

    !! (nugetDir + sprintf "Cashel.%s.nupkg" version)
        |> CopyTo deployDir
)

FinalTarget "CloseTestRunner" (fun _ ->
    ProcessHelper.killProcess "nunit-agent.exe"
)

Target "Deploy" DoNothing
Target "Default" DoNothing

// Dependencies
"Clean"
    ==> "BuildApp"
    ==> "BuildTest"
    ==> "Test"
    ==> "CopyLicense"
    ==> "BuildZip"
    ==> "CreateNuGet"
    ==> "Deploy"

"Default" <== ["Deploy"]

// start build
RunTargetOrDefault "Default"

