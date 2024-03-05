export {};

declare global {
  interface UserProfile {
    id: number;
    name: string;
    role_id: number;
    is_admin: boolean;
    is_liaison: boolean;
    is_coc_primary: boolean;
    modules: string[];
  }
}
