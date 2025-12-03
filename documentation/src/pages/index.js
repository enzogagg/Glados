import React from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import styles from '../css/index.module.css';

function HomepageHeader() {
    const { siteConfig } = useDocusaurusContext();
    return (
        <header className={clsx('hero hero--primary', styles.heroBanner)} style={{ backgroundColor: '#1b1b1d', backgroundImage: 'linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5))', backgroundSize: 'cover', backgroundPosition: 'center' }}>
            <div className="container">
                <h1 className="hero__title" style={{ fontSize: '4rem', fontWeight: 'bold' }}>{siteConfig.title}</h1>
                <p className="hero__subtitle" style={{ fontSize: '1.5rem' }}>{siteConfig.tagline}</p>
                <div className={styles.buttons}>
                    <Link
                        className="button button--secondary button--lg"
                        to="/docs/Introduction">
                        Commencer les Tests 🧪
                    </Link>
                </div>
            </div>
        </header>
    );
}

function Feature({ title, description }) {
    return (
        <div className={clsx('col col--4')}>
            <div className={clsx('text--center', styles.feature)}>
                <h3>{title}</h3>
                <p>{description}</p>
            </div>
        </div>
    );
}

export default function Home() {
    const { siteConfig } = useDocusaurusContext();
    return (
        <Layout
            title={`Bienvenue sur ${siteConfig.title}`}
            description="Documentation GLaDOS">
            <HomepageHeader />
            <main>
                <section style={{ padding: '4rem 0' }}>
                    <div className="container">
                        <div className="row">
                            <Feature
                                title="Syntaxe de Test Optimisée"
                                description="Une syntaxe expressive inspirée du Lisp, conçue pour une efficacité de test maximale dans les chambres de test."
                            />
                            <Feature
                                title="Propulsé par Haskell"
                                description="Construit avec la robustesse d'Haskell, garantissant une expérience sans glitch (la plupart du temps)."
                            />
                            <Feature
                                title="Protocole Interactif"
                                description="Testez votre code en temps réel avec notre boucle de lecture-évaluation-impression avancée. Gâteau non inclus."
                            />
                        </div>
                    </div>
                </section>
                <div style={{ textAlign: 'center', padding: '2rem', backgroundColor: '#f5f6f7', color: '#1b1b1d' }}>
                    <h2>Le Centre d'Enrichissement s'engage au bien-être de tous les participants.</h2>
                    <p>Rappel : Le gâteau est un mensonge.</p>
                </div>
            </main>
        </Layout>
    );
}