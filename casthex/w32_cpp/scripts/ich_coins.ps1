# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to generate an I-Ching casting via the coin-toss method.  Suitable for
# piping into ich_display.exe
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

$t1 = 6 + (Get-Random -Maximum 2) + (Get-Random -Maximum 2) + (Get-Random -Maximum 2) 
$t2 = 6 + (Get-Random -Maximum 2) + (Get-Random -Maximum 2) + (Get-Random -Maximum 2) 
$t3 = 6 + (Get-Random -Maximum 2) + (Get-Random -Maximum 2) + (Get-Random -Maximum 2) 
$t4 = 6 + (Get-Random -Maximum 2) + (Get-Random -Maximum 2) + (Get-Random -Maximum 2) 
$t5 = 6 + (Get-Random -Maximum 2) + (Get-Random -Maximum 2) + (Get-Random -Maximum 2) 
$t6 = 6 + (Get-Random -Maximum 2) + (Get-Random -Maximum 2) + (Get-Random -Maximum 2) 

Write-Output $t1$t2$t3$t4$t5$t6

