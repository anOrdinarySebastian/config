for($i = 1; $i -le 5; $i++){
  $u =  "".PadLeft($i,"u")
  $unum =  "u$i"
  $d =  $u.Replace("u","../")
  Invoke-Expression "function $u { push-location $d }"
  Invoke-Expression "function $unum { push-location $d }"
}

function Get-Git-Branch () {
         try {
             # Get the name of the current head branch (?)
             $branch = git rev-parse --abbrev-ref HEAD

             if ( $branch -eq "HEAD") {
             # Probably because of DETACHED HEAD, print the SHA
               # get SHA
               $branch = git rev-parse --short HEAD
               Write-Host -NoNewLine "($branch)" -ForegroundColor Red
             } else {
               Write-Host -NoNewLine "($branch)" -ForegroundColor Yellow
             }
         } catch {
           # If the branch varaible failed then we're probably not in a repo
           Write-Host -NoNewLine "" -ForegroundColor Yellow
         }

}

function prompt {
Write-Host -NoNewLine "PS " -ForegroundColor DarkBlue
Write-Host -NoNewLine "($(Get-Date -Format hh:mm)) " -ForegroundColor white
Write-Host -NoNewLine "$($executionContext.SessionState.Path.CurrentLocation) " -ForegroundColor Red
if (Test-Path .git) {
      Get-Git-Branch
    }
Write-host -NoNewLine "$('>' * ($nestedPromptLevel + 1))"
return " "
}

New-PSDrive -Name plan -PSProvider FileSystem -Root 'C:\Users\sebbe\git\datx05\61e68963dab8fe6667ee6fd9\' -Description "Mapping of the datx05 planning report to a nicer name"
New-PSDrive -Name report -PSProvider FileSystem -Root 'C:\Users\sebbe\git\datx05\616540e313997595df739a1e\' -Description "Mapping of the datx05 final report to a nicer name"