import type { UseFetchOptions } from "#app";
import { useSessionStorage, StorageSerializers } from "@vueuse/core";

export const useFetchWithCache = async <T>(url: string, options: UseFetchOptions<T> = {}) => {
  // use sessionStorage to cache data
  const cached = useSessionStorage<T>(url, null, { serializer: StorageSerializers.object });

  if (!cached.value) {
    const { data, error } = await useFetch(url, options);
    if (error.value) {
      throw createError({
        ...error.value,
        statusMessage: `Could not fetch data from ${url}`,
      });
    }
    // update the cache
    cached.value = data.value as T;
  } else {
    console.log(`Getting value from cache for ${url}`);
  }

  return cached;
};
