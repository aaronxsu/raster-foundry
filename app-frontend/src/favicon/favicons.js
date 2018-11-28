/* globals BUILDCONFIG */
const faviconsContext = require.context(
    `!!file-loader?name=favicons/[name].[ext]!..${BUILDCONFIG.FAVICON_DIR || '/favicon'}`,
    true,
    /\.(svg|png|ico|xml)$/
);
faviconsContext.keys().forEach(faviconsContext);

// const manifest = require.context(
//     // `..${BUILDCONFIG.FAVICON_DIR || '/favicon'}/manifest.json`,
//     `!!json-loader?name=favicons/[name].[ext]!..${BUILDCONFIG.FAVICON_DIR || '/favicon'}`,
//     true,
//     /\.json$/
// );