module.exports = function (grunt) {
    var CSS_FILES = [
        'static/style.css',
        'static/syntax.css',
        'static/wilfred.css'
    ];
    
    grunt.initConfig({
        pkg: grunt.file.readJSON('package.json'),
        concat: {
            css: {
                src: CSS_FILES,
                dest: 'static/combined.css'
            }
        },
        autoprefixer: {
            options: {
                safe: true
            },
            single_file: {
                src: 'static/combined.css',
                dest: 'static/prefixed.css'
            }
        },
        cssmin: {
            css: {
                src: 'static/prefixed.css',
                dest: 'static/min.css'
            }
        },
        clean: ['static/combined.css',
                'static/prefixed.css'
               ],
        watch: {
            files: CSS_FILES,
            tasks: ['default']
        }
    });
    grunt.loadNpmTasks('grunt-contrib-concat');
    grunt.loadNpmTasks('grunt-contrib-watch');
    grunt.loadNpmTasks('grunt-contrib-cssmin');
    grunt.loadNpmTasks('grunt-contrib-clean');
    grunt.loadNpmTasks('grunt-autoprefixer');
    grunt.registerTask('default',
                       ['concat:css', 'autoprefixer', 'cssmin:css', 'clean']);
};
