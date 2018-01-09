hooks_scripts=$(find -P git_maintenance/hooks ! -path git_maintenance/hooks/hooks.cl -a  ! -path git_maintenance/hooks/.*.swp -xtype f -print)
for script in  $hooks_scripts
do
    ln -s -r  $script .git/hooks/
done
