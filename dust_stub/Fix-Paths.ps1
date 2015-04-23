$dust_installed_at="c:\dusttest\"
$dust_old_path="c:\MyWork\dust\dust-install\app_files\"

$current_user=[System.Security.Principal.WindowsIdentity]::GetCurrent()
$primary_group=$current_user.Groups[0]
$full_access= New-Object  system.security.accesscontrol.filesystemaccessrule($current_user.Name,"FullControl","Allow")

$princ=new-object System.Security.Principal.WindowsPrincipal($current_user)
$adm_role=[System.Security.Principal.WindowsBuiltInRole]::Administrator
$is_admin=$princ.IsInRole($adm_role)
 

dir $dust_installed_at\DustData\*.DAT | Foreach-Object { 
    Set-ItemProperty $_ -name IsReadOnly -value $false
    
    if ($is_admin) {
        $acl=Get-Acl $_
        $acl.SetOwner($primary_group)
        $acl.SetAccessRule($full_access)
        Set-Acl $_ $acl
    }
        
       
    
    $new_content=(Get-Content $_ | %{ $_ -ireplace [regex]::escape("$dust_old_path"),"$dust_installed_at" })        
    Set-Content $_ -Value $new_content  
} 
