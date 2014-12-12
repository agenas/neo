/*
 * Copyright 2014 Stefano Gualdi, AGENAS.
 *
 * Licensed under the European Union Public Licence (EUPL), Version 1.1 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * @author Stefano Gualdi <stefano.gualdi@gmail.com>
 */

// key signing information
environments {
    development {
        signingkey {
            params {
                // sigfile = 'GRIFFON'
                // keystore = "${basedir}/griffon-app/conf/keys/devKeystore"
                // alias = 'development'
                storepass = 'BadStorePassword'
                keypass   = 'BadKeyPassword'
                lazy      = true // only sign when unsigned
            }
        }
    }
    test {
        griffon {
            jars {
                sign = false
                pack = false
            }
        }
    }
    production {
        signingkey {
            params {
                // NOTE: for production keys it is more secure to rely on key prompting
                // no value means we will prompt //storepass = 'BadStorePassword'
                // no value means we will prompt //keypass   = 'BadKeyPassword'
                lazy = false // sign, regardless of existing signatures
            }
        }

        griffon {
            jars {
                sign = false
                pack = false
                destDir = "${basedir}/staging"
            }
            webstart {
                codebase = 'CHANGE ME'
            }
        }
    }
}

// griffon.packaging = ['dmg', 'jsmooth']
griffon.packaging = ['jsmooth', 'mac', 'izpack']

griffon {
    memory {
        max = '1024m'
        min = '512m'
        //minPermSize = '2m'
        //maxPermSize = '64m'
    }
    dist {
        jar {
            nozip = true
        }
    }
    jars {
        sign = false
        pack = false
        destDir = "${basedir}/staging"
        jarName = "${appName}.jar"
    }
    extensions {
        jarUrls = []
        jnlpUrls = []
        /*
        props {
            someProperty = 'someValue'
        }
        resources {
            linux { // windows, macosx, solaris
                jars = []
                nativelibs = []
                props {
                    someProperty = 'someValue'
                }
            }
        }
        */
    }
    webstart {
        codebase = "${new File(griffon.jars.destDir).toURI().toASCIIString()}"
        jnlp = 'application.jnlp'
    }
    applet {
        jnlp = 'applet.jnlp'
        html = 'applet.html'
    }
}

// required for custom environments
signingkey {
    params {
        def env = griffon.util.Environment.current.name
        sigfile = 'GRIFFON-' + env
        keystore = "${basedir}/griffon-app/conf/keys/${env}Keystore"
        alias = env
        // storepass = 'BadStorePassword'
        // keypass   = 'BadKeyPassword'
        lazy      = true // only sign when unsigned
    }
}

griffon {
    doc {
        logo = '<a href="http://griffon-framework.org" target="_blank"><img alt="The Griffon Framework" src="../img/griffon.png" border="0"/></a>'
        sponsorLogo = "<br/>"
        footer = "<br/><br/>Made with Griffon"
    }
}

deploy {
    application {
        title = "${appName} ${appVersion}"
        vendor = System.properties['user.name']
        homepage = "http://localhost/${appName}"
        description {
            complete = "${appName} ${appVersion}"
            oneline  = "${appName} ${appVersion}"
            minimal  = "${appName} ${appVersion}"
            tooltip  = "${appName} ${appVersion}"
        }
        icon {
            'default' {
                name   = 'neobox-icon-64x64.png'
                width  = '64'
                height = '64'
            }
            splash {
                name   = 'neobox-splash.png'
                width  = '391'
                height = '123'
            }
            selected {
                name   = 'neobox-icon-64x64.png'
                width  = '64'
                height = '64'
            }
            disabled {
                name   = 'neobox-icon-64x64.png'
                width  = '64'
                height = '64'
            }
            rollover {
                name   = 'neobox-icon-64x64.png'
                width  = '64'
                height = '64'
            }
            shortcut {
                name   = 'neobox-icon-64x64.png'
                width  = '64'
                height = '64'
            }
        }
    }
}

griffon.project.dependency.resolution = {
    // inherit Griffon' default dependencies
    inherits("global") {
    }
    
    log "warn" // log level of Ivy resolver, either 'error', 'warn', 'info', 'debug' or 'verbose'
    repositories {
        griffonHome()

        // uncomment the below to enable remote dependency resolution
        // from public Maven repositories
        mavenLocal()
        mavenCentral()

        mavenRepo "http://nexus.bedatadriven.com/content/groups/public/"
        mavenRepo "http://docbook4j.googlecode.com/svn/m2-repo/releases/"
    }

    dependencies {
        // specify dependencies here under either 'build', 'compile', 'runtime' or 'test' scopes eg.
        compile 'com.h2database:h2:1.3.170'
        compile 'org.yaml:snakeyaml:1.13'
        compile 'com.google.code.docbook4j:docbook4j:1.0.0'

        runtime 'commons-lang:commons-lang:2.6'
        runtime 'commons-net:commons-net:3.3'

        // compile 'ognl:ognl:3.0.5'
        // compile 'org.mvel:mvel2:2.1.0.drools16'
    }
}

log4j = {
    // Example of changing the log pattern for the default console
    // appender:
    appenders {
        console name: 'stdout', layout: pattern(conversionPattern: '%d [%t] %-5p %c - %m%n')
    }

    error 'org.codehaus.griffon',
          'org.springframework',
          'org.apache.karaf',
          'groovyx.net'
    warn  'griffon'
}


app.fileType = '.groovy'
app.defaultPackageName = 'neobox'

application.icon = '/griffon-app/conf/dist/shared/neobox.icns'

griffon.project.source.level = '1.7'
griffon.project.target.level = '1.7'
