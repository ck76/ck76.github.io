import React from 'react';
import styled from 'styled-components';
import { useStaticQuery, graphql } from 'gatsby';
import GatsbyImage from 'gatsby-image';
import Layout from '../../../components/Layout';
import PostList from '../../../components/PostList';

const FullImage = styled(GatsbyImage)`
  max-width: 600px;
  height: 400px;
  margin: 0 auto 30px auto;
  border-radius: 5px;
`;
const ImageContent = styled.div`
  position: relative;
`;

const ImageTitle = styled.h1`
  display: flex;
  width: 100%;
  height: 100%;
  justify-content: center;
  align-items: center;
  position: absolute;
  z-index: 1;
  margin: 0;
  color: var(--white);
`;

const RxjsPages = () => {
  const { allMarkdownRemark, file } = useStaticQuery(graphql`
    query {
      file(relativePath: { eq: "rxjs-thirtydays.png" }) {
        childImageSharp {
          fluid(maxWidth: 600, quality: 60) {
            ...GatsbyImageSharpFluid
          }
        }
      }
      allMarkdownRemark(
        limit: 1000
        sort: { fields: frontmatter___date, order: ASC }
        filter: { frontmatter: { templateKey: { eq: "series" }, series: { eq: "30 天精通 RxJS" } } }
      ) {
        edges {
          node {
            id
            timeToRead
            fields {
              slug
            }
            frontmatter {
              tags
              templateKey
              title
              description
              date(formatString: "MMM Do, YYYY")
              image {
                childImageSharp {
                  fluid(maxWidth: 770, quality: 64) {
                    ...GatsbyImageSharpFluid
                  }
                }
              }
            }
          }
        }
      }
    }
  `);

  return (
    <Layout header="Series / 30 天精通 RxJS">
      <ImageContent>
        <ImageTitle>30 天精通 RxJS</ImageTitle>
        <FullImage fluid={file.childImageSharp.fluid} />
      </ImageContent>
      <PostList data={allMarkdownRemark.edges} />
    </Layout>
  );
};

export default RxjsPages;
