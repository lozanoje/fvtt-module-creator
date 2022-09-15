Hooks.once('renderCompendiumDirectory', () => {

    game.settings.register('compendium-folders', 'restablecer-compendios', {
        name: 'Restablecer carpetas de compendios',
        hint: 'Al marcarla restablecera las carpetas de los compendios a su configuracion inicial',
        type: Boolean,
        default: true,
        scope: 'world',
        config: true,
        onChange: directory => {
            window.location.reload();
        }
    });
    
    document.onkeyup = function(e) {
        if (e.ctrlKey && e.altKey && e.which == 89) {
            initCompendiums();
        }
    }
    
    if (game.user.isGM && game.settings.get('compendium-folders', 'restablecer-compendios')){
        initCompendiums();
    }
  
    
    function initCompendiums(){
        let compendiumFolderJson =JSON.parse(`@~@`);

        
            setTimeout(()=>{
            game.settings.set('compendium-folders','cfolders',compendiumFolderJson).then(function(){
                                    ui.notifications.info("Actualizadas carpetas de compendio, refresque la pestaña");
                                });
            game.settings.set('compendium-folders', 'restablecer-compendios', false);
        }, 2000);
    }
    
    

});
