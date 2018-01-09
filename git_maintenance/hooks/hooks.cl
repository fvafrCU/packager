hooks_scripts=$(find -P hooks ! -path git_maintenance/hooks/hooks.cl -a  ! -path hooks/.*.swp -xtype f -print)
for script in  $hooks_scripts
do
    ln -s -r  $script .git/hooks/
done
