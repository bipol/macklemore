var versionCtrl = Date.parse(new Date().toISOString());

module.exports = {
    css: {
        src: './sass/*.scss',
        dest: '../public',
        config: {
            outputstyle: 'expanded'
        }
    },

    /*icons:{

        src: './icons*//*.png',
        imgDest: './web',
        cssDest: './sass',
        oldPath: "./web/icons-*.png",
        iconConfig: {
            imgName: 'icons-' + versionCtrl + '.png',
            cssName: '_icons.scss',
            cssTemplate: './sass/sprite-template.scss.mustache',
            padding: 5,
            cssVarMap: function(item) {
                if (item.name.indexOf('_hover') !== -1) {
                    item.name = 'icon-' + item.name + ', .icon-' + item.name.replace('_hover', ':hover');
                } else {
                    item.name = 'icon-' + item.name;
                }
            }
        }

    },*/



};