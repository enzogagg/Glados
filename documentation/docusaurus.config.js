// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
    title: 'GLaDOS',
    tagline: 'Generic Language and Data Operand Syntax',
    favicon: 'img/favicon.ico',

    url: 'https://enzogagg.github.io', // Your GitHub Pages base URL
    baseUrl: '/Glados/',              // The exact name of your repository (ex: /G-FUN-500.../)

    // GitHub Pages deployment config
    organizationName: 'enzogagg', // Your GitHub username
    projectName: 'Glados',       // The exact name of your repository
    deploymentBranch: 'gh-pages',
    trailingSlash: false,

    onBrokenLinks: 'throw',
    onBrokenMarkdownLinks: 'warn',

    i18n: {
        defaultLocale: 'en',
        locales: ['en'],
    },

    presets: [
        [
            'classic',
            /** @type {import('@docusaurus/preset-classic').Options} */
            ({
                docs: {
                    sidebarPath: require.resolve('./sidebars.js'),
                    // Edit this page link
                    editUrl:
                        'https://github.com/enzogagg/Glados/tree/main/documentation/',
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
            // Social image
            image: 'img/docusaurus-social-card.jpg',
            navbar: {
                title: 'GLaDOS',
                logo: {
                    alt: 'GLaDOS Logo',
                    src: 'img/logo.svg',
                },
                items: [
                    // User Manual
                    {
                        type: 'docSidebar',
                        sidebarId: 'tutorialSidebar',
                        position: 'left',
                        label: 'User Manual',
                    },
                    // Developer API (Haddock)
                    {
                        to: 'pathname:///api/index.html',
                        label: 'Developer API (Haddock)',
                        position: 'left',
                    },
                    // GitHub
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
                        title: 'Docs',
                        items: [
                            {
                                label: 'User Manual',
                                to: '/docs/intro',
                            },
                            {
                                label: 'Developer API',
                                href: 'pathname:///api/index.html',
                            },
                        ],
                    },
                    {
                        title: 'Community',
                        items: [
                            {
                                label: 'Epitech',
                                href: 'https://epitech.eu',
                            },
                        ],
                    },
                ],
                copyright: `Copyright © ${new Date().getFullYear()} GLaDOS Project. Built with Docusaurus & Haskell.`,
            },
            prism: {
                theme: lightCodeTheme,
                darkTheme: darkCodeTheme,
                // Add support for Haskell and Lisp/Scheme syntax highlighting
                additionalLanguages: ['haskell', 'scheme', 'lisp', 'makefile', 'bash'],
            },
        }),
};

module.exports = config;