// https://nuxt.com/docs/api/configuration/nuxt-config
export default defineNuxtConfig({
  runtimeConfig: {
    public: {
      hdxBase: "https://hdxdev.abtsites.com",
      hicBase: "https://hdxdev-modules.abtsites.com/hic",
      pitBase: "https://hdxdev-modules.abtsites.com/pit",
      spmBase: "https://hdxdev-modules.abtsites.com/spm",
      stpBase: "https://hdxdev-stellap.abtsites.com",
      stmBase: "https://hdxdev-stellam.abtsites.com",
    },
  },

  modules: ["@nuxt/ui"],

  imports: {
    dirs: ["types/*.d.ts"],
  },

  devtools: { enabled: true },
});
