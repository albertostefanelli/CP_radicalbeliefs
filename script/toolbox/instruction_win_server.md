## Run Mplus on Win Server 2019 for LPA 

1. AWS server Win with core >= 8
2. RDP connection with open ports 
3. Install following SW:
	1. Chrome (Explorer has security enabled) using power shell
	$LocalTempDir = $env:TEMP; $ChromeInstaller = "ChromeInstaller.exe"; (new-object    System.Net.WebClient).DownloadFile('http://dl.google.com/chrome/install/375.126/chrome_installer.exe', "$LocalTempDir\$ChromeInstaller"); & "$LocalTempDir\$ChromeInstaller" /silent /install; $Process2Monitor =  "ChromeInstaller"; Do { $ProcessesFound = Get-Process | ?{$Process2Monitor -contains $_.Name} | Select-Object -ExpandProperty Name; If ($ProcessesFound) { "Still running: $($ProcessesFound -join ', ')" | Write-Host; Start-Sleep -Seconds 2 } else { rm "$LocalTempDir\$ChromeInstaller" -ErrorAction SilentlyContinue -Verbose } } Until (!$ProcessesFound)
	2. Install R
	3. Install RTools (for compiling packages if needed)
	3. Install Rstudio
4. Copy Mplus_win from ICloud
5. Copy entire github project directory and open directly in RStudio
6. Modify the function_mplus_syntax.R 
	1. line 356 CHANGE N processors in mplusObject() function
	2. Mplus_command specification for WIN in runModels() function. Line 389 REPLACE: runModels(target=here(name_base,name_run_models), recursive = FALSE, replaceOutfile="modifiedDate", showOutput = FALSE,  Mplus_command = "C:/Users/Administrator/Desktop/Mplus_win/Mplus.exe")
 