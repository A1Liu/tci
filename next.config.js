/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  output: "export",
  webpack: function (config, options) {
    config.experiments = {
      asyncWebAssembly: true,

      // https://github.com/vercel/next.js/issues/29362
      layers: true,
    };
    return config;
  },
};

module.exports = nextConfig;
