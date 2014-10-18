module.exports = function (grunt) {
    grunt.initConfig({
        pkg: grunt.file.readJSON('package.json'),
        concat: {
            css: {
                src: [
                    'static/style.css',
                    'static/syntax.css',
                    'static/wilfred.css'
                ],
                dest: 'static/combined.css'
            }
        },
        cssmin: {
            css: {
                src: 'static/combined.css',
                dest: 'static/min.css'
            }
        },
        clean: ['static/combined.css']
    });
    grunt.loadNpmTasks('grunt-contrib-concat');
    grunt.loadNpmTasks('grunt-contrib-watch');
    grunt.loadNpmTasks('grunt-contrib-cssmin');
    grunt.loadNpmTasks('grunt-contrib-clean');
    grunt.registerTask('default', ['concat:css', 'cssmin:css', 'clean']);
};
