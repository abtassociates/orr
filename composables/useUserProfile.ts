export const useUserProfile = async () => {
  // get access token from cookie
  const access_token = useCookie("access_token", { watch: false });
  const token = process.env.NODE_ENV === "development" ? useRuntimeConfig().public.orrDevToken : access_token.value;

  const profile = await useFetchWithCache<UserProfile>(`${useRuntimeConfig().public.orrApiUrl}/user/profile`, {
    headers: { Authorization: `Bearer ${token}` },
  });

  // make it readonly
  return readonly(profile);
};
