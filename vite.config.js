import { defineConfig } from "vite";
import { VitePWA } from "vite-plugin-pwa";

export default defineConfig({
  base: "./",
  server: { host: true },
  plugins: [
    VitePWA({
      injectRegister: "auto",
      registerType: "autoUpdate",
      includeAssets: ["icons/apple-touch-icon.png", "icons/favicon-64.png"],
      manifest: {
        id: "./",
        name: "Alphababy",
        short_name: "Alphababy",
        description: "Sparkly letter, sound and number games for little ones who are learning to read.",
        start_url: "./",
        scope: "./",
        display: "fullscreen",
        display_override: ["fullscreen", "standalone"],
        orientation: "any",
        background_color: "#2a0a45",
        theme_color: "#2a0a45",
        categories: ["education", "kids", "games"],
        icons: [
          { src: "icons/icon-192.png", sizes: "192x192", type: "image/png", purpose: "any" },
          { src: "icons/icon-512.png", sizes: "512x512", type: "image/png", purpose: "any" },
          { src: "icons/icon-maskable-512.png", sizes: "512x512", type: "image/png", purpose: "maskable" },
        ],
      },
      workbox: {
        cleanupOutdatedCaches: true,
        globPatterns: ["**/*.{css,html,js,png,svg,woff,woff2}"],
        navigateFallback: "index.html",
      },
    }),
  ],
});
