
var sass = require('gulp-sass'),
    gulp = require('gulp'),
    del = require('del'),
    sourcemaps = require('gulp-sourcemaps'),
    autoprefix = require('gulp-autoprefixer'),
    config = require('./gulp-config');

//SASS Processing


gulp.task('css', function() {
    //var uglyLevel = debug ? true : false;
    //var proDate = new Date().toString;

    return gulp.src(config.css.src)

        .pipe(sourcemaps.init())
        .pipe(sass(config.css.config))
        .pipe(autoprefix({
            browsers:["last 2 versions"]
        }))
        .pipe(sourcemaps.write())
        //Defines destination of css files
        .pipe(gulp.dest(config.css.dest));


});

/*


*/
/*Sprite Tasks*//*


function iconSass() {

    return gulp.src(config.css.src)

        //Defines the Source .scss files
        .pipe(sourcemaps.init())
        .pipe(sass(config.css.config) )
        .pipe(autoprefix({
            browsers:["last 2 versions"]
        }))
        .pipe(sourcemaps.write())
        //Defines destination of css files
        .pipe(gulp.dest(config.css.dest));

}


gulp.task('icons', function(cb) {

    del([config.icons.oldPath],{force:true}, function (err, deletedFiles) {
        gutil.log(gutil.colors.cyan('Sprite files deleted: ', deletedFiles.join(', ')));
    });
    var spriteData = gulp.src(config.icons.src)
        .pipe(spritesmith(config.icons.iconConfig));

    spriteData.img.pipe(gulp.dest(config.icons.imgDest));
    return spriteData.css.pipe(gulp.dest(config.icons.cssDest))
        .on('end', function() {
            iconSass()
        });
});*/
