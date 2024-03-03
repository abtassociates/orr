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
      orrApiUrl: "https://lhbxd6zf10.execute-api.us-east-1.amazonaws.com",
      orrDevToken: "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJhdWQiOiIyIiwianRpIjoiYzY0YWEyMGY1NWI1ZGZiNGI0M2M1OTMyN2U2MGI2YzVjZjMyMDkyNjhhZWFiYTdmZjIwYmE1ZWIwOWQ0ODFiNGYxZDM4M2YxM2E5NzliZmQiLCJpYXQiOjE3MDk1MDA2NjkuNzg4MDY2LCJuYmYiOjE3MDk1MDA2NjkuNzg4MDY4LCJleHAiOjE3MTI3NTUxMjAsInN1YiI6IjEifQ.WE0H-HRZrOllq4bnt0p1IPLpnrIOBHvrXupP4gbwNBPonfIBPfOnSjlMF9FEhQFwFcCWIE2My4z4oM-2EVR2z8BI9PjvVzcafxl2mfSl1DMZoWu6Lze53N_i4rfZkRclfKh3gPSi08yvAL09MZsefO0cKEn9sD_HU0HBc6IqXpO4SlEfzraN4etcj-g4_FVrGB-ffQPpZQJYpG9n4PywfuDuBV3BX5lE1fJX90lytKO6n1TI5i83_hRWuPjIaNEXTIQsgCrpnkduiPyLFoAsWKtxNrLnp4lAkhRZ7Qlak-KRoJgwloPHCQgWUQei5IFd3CgEl2JpnET3OdMee7NfTgsFRyfUFKp7ClMj9vM_2-OFPwop4xXZkoeaZpiH2E8pDCdcsro_aC5hgz6JT__iwPxJN66tpon3kY3uiFgYwFLiSTOeFwYflmsW081HUPUoIasooOH_Tp0hNnb37txWadELo7gTYJ-3AXzHC_ZaaS1G3wOsmXUYowBgZpqvCOfQ892QixVSPVde6STaqWOLxRBkBogeHjc60UDDyNsBWIvTzenSyScAQUzNm3bycFaxjrQ0AftKsEaZwyj4f3ZFenND-R4n-acHC_BxPI0dcd6cxkHKFLVUJhrKazk5sRSrYQTT-9LSAQtZl4pgaCzIqzdfapD5MaZ2QMXBxGBbpoE",
    },
  },

  modules: ["@nuxt/ui"],

  imports: {
    dirs: ["types/*.d.ts"],
  },

  devtools: { enabled: true },
});
