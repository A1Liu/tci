/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  output: "export",
  webpack: function (config, options) {
    config.experiments = { asyncWebAssembly: true };
    return config;
  },
};

module.exports = nextConfig;
