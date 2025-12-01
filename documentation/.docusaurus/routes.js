import React from 'react';
import ComponentCreator from '@docusaurus/ComponentCreator';

export default [
  {
    path: '/Glados/__docusaurus/debug',
    component: ComponentCreator('/Glados/__docusaurus/debug', 'd76'),
    exact: true
  },
  {
    path: '/Glados/__docusaurus/debug/config',
    component: ComponentCreator('/Glados/__docusaurus/debug/config', '5a8'),
    exact: true
  },
  {
    path: '/Glados/__docusaurus/debug/content',
    component: ComponentCreator('/Glados/__docusaurus/debug/content', '2d0'),
    exact: true
  },
  {
    path: '/Glados/__docusaurus/debug/globalData',
    component: ComponentCreator('/Glados/__docusaurus/debug/globalData', '1a8'),
    exact: true
  },
  {
    path: '/Glados/__docusaurus/debug/metadata',
    component: ComponentCreator('/Glados/__docusaurus/debug/metadata', 'f29'),
    exact: true
  },
  {
    path: '/Glados/__docusaurus/debug/registry',
    component: ComponentCreator('/Glados/__docusaurus/debug/registry', '7b0'),
    exact: true
  },
  {
    path: '/Glados/__docusaurus/debug/routes',
    component: ComponentCreator('/Glados/__docusaurus/debug/routes', 'bc0'),
    exact: true
  },
  {
    path: '/Glados/docs',
    component: ComponentCreator('/Glados/docs', '22a'),
    routes: [
      {
        path: '/Glados/docs',
        component: ComponentCreator('/Glados/docs', 'bc6'),
        routes: [
          {
            path: '/Glados/docs',
            component: ComponentCreator('/Glados/docs', 'cfe'),
            routes: [
              {
                path: '/Glados/docs/introduction',
                component: ComponentCreator('/Glados/docs/introduction', '37a'),
                exact: true,
                sidebar: "tutorialSidebar"
              },
              {
                path: '/Glados/docs/types-et-mots-cles',
                component: ComponentCreator('/Glados/docs/types-et-mots-cles', '8c2'),
                exact: true,
                sidebar: "tutorialSidebar"
              }
            ]
          }
        ]
      }
    ]
  },
  {
    path: '/Glados/',
    component: ComponentCreator('/Glados/', '1cd'),
    exact: true
  },
  {
    path: '*',
    component: ComponentCreator('*'),
  },
];
