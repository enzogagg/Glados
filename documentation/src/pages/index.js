import React from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';

function HomepageHeader() {
    const { siteConfig } = useDocusaurusContext();
    return (
        <header style={{
            padding: '4rem 0',
            textAlign: 'center',
            backgroundColor: 'var(--ifm-color-primary)',
            color: 'white'
        }}>
            <div className="container">
                <h1 className="hero__title">{siteConfig.title}</h1>
                <p className="hero__subtitle">{siteConfig.tagline}</p>
                <div style={{ marginTop: '2rem' }}>
                    <Link
                        className="button button--secondary button--lg"
                        to="/docs/Introduction">
                        Accéder au Manuel Utilisateur 📚
                    </Link>
                </div>
            </div>
        </header>
    );
}

export default function Home() {
    const { siteConfig } = useDocusaurusContext();
    return (
        <Layout
            title={`Accueil`}
            description="Documentation du projet GLaDOS">
            <HomepageHeader />
            <main style={{ padding: '2rem', textAlign: 'center' }}>
                <h2>The Enrichment Center is committed to the well-being of all participants.</h2>
                <p>
                    Cette documentation contient tout ce dont vous avez besoin pour utiliser
                    l'interpréteur GLaDOS.
                </p>
            </main>
        </Layout>
    );
}