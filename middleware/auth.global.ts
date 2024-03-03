import { useUserProfile } from "~/composables/useUserProfile";

export default defineNuxtRouteMiddleware(async (to, from) => {
  const profile = await useUserProfile();

  if (typeof profile.value === "object" && "id" in profile.value) {
    // all good
  } else {
    return navigateTo(useRuntimeConfig().public.hdxBase, { external: true });
  }
});
