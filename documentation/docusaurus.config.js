// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const { themes } = require('prism-react-renderer');
const lightCodeTheme = themes.github;
const darkCodeTheme = themes.dracula;

/** @type {import('@docusaurus/types').Config} */
const config = {
    title: 'ClaD',
    tagline: 'Generic Language and Data Operand Syntax',
    favicon: 'img/favicon.ico',

    // Configuration GitHub Pages
    url: 'https://enzogagg.github.io',
    baseUrl: process.env.DOCS_BASE_URL || '/',
    organizationName: 'enzogagg',
    projectName: 'Glados',
    deploymentBranch: 'gh-pages',
    trailingSlash: false,

    onBrokenLinks: 'throw',
    onBrokenMarkdownLinks: 'warn',

    // --- CONFIGURATION I18N CORRIGÉE ---
    i18n: {
        defaultLocale: 'fr',    // Tes fichiers originaux (docs/) sont en Français
        locales: ['fr', 'en'],  // On active le Français et l'Anglais
        localeConfigs: {
            fr: {
                label: 'Français',
            },
            en: {
                label: 'English',
            },
        },
    },

    presets: [
        [
            'classic',
            /** @type {import('@docusaurus/preset-classic').Options} */
            ({
                docs: {
                    sidebarPath: require.resolve('./sidebars.js'),
                    // IMPORTANT : Ceci met la doc à la racine du site
                    routeBasePath: '/', 
                    editUrl: 'https://github.com/enzogagg/Glados/tree/main/documentation/',
                },
                blog: false,
                theme: {
                    customCss: require.resolve('./src/css/custom.css'),
                },
            }),
        ],
    ],

    themeConfig:
        /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
        ({
            image: 'img/logo.png',
            announcementBar: {
                id: 'cake_is_a_lie',
                content: '🍰 <strong>Welcome to the GLaDOS Documentation.</strong> 🍰',
                backgroundColor: '#245d88',
                textColor: '#fff',
                isCloseable: true,
            },
            navbar: {
                title: 'GLaDOS',
                logo: {
                    alt: 'GLaDOS Logo',
                    src: 'img/logo.png',
                },
                items: [
                    // --- MENU DE NAVIGATION ---
                    {
                        type: 'docSidebar',
                        sidebarId: 'tutorialSidebar',
                        position: 'left',
                        label: 'Manuel', // Label par défaut (affiché en FR)
                    },
                    {
                        to: 'pathname:///api/index.html',
                        label: 'API Reference',
                        position: 'left',
                    },
                    // --- BOUTON DE LANGUE (IMPORTANT) ---
                    {
                        type: 'localeDropdown',
                        position: 'right',
                    },
                    // --- LIEN GITHUB ---
                    {
                        href: 'https://github.com/enzogagg/Glados',
                        label: 'GitHub',
                        position: 'right',
                    },
                ],
            },
            footer: {
                style: 'dark',
                links: [
                    {
                        title: 'Documentation',
                        items: [
                            {
                                label: 'Manuel Utilisateur',
                                to: '/', // Corrigé pour pointer vers la racine
                            },
                            {
                                label: 'API Reference',
                                href: 'pathname:///api/index.html',
                            },
                        ],
                    },
                    {
                        title: 'Communauté',
                        items: [
                            {
                                label: 'Epitech',
                                href: 'https://epitech.eu',
                            },
                            {
                                label: 'GitHub',
                                href: 'https://github.com/enzogagg/Glados',
                            },
                        ],
                    },
                ],
                copyright: `Copyright © ${new Date().getFullYear()} CLaD Project. Built with Docusaurus & Haskell.`,
            },
            prism: {
                theme: lightCodeTheme,
                darkTheme: darkCodeTheme,
                additionalLanguages: ['haskell', 'scheme', 'lisp', 'makefile', 'bash'],
            },
        }),
};

module.exports = config;