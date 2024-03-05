import fetch from "node-fetch";
import _ from "lodash";
import * as utils from "./utils.mjs";

export const getUserProfile = async (event) => {
  // check bearer token
  if (!("authorization" in event.headers && event.headers.authorization.startsWith("Bearer"))) {
    return utils.httpResponse(401, "Unauthorized");
  }

  // get base user profile
  let profile = {};
  const response = await fetch(`${process.env.LSA_WEBSITE}/api/auth/user`, {
    headers: { Authorization: event.headers.authorization },
  });
  try {
    profile = await response.json();
  } catch (error) {
    return utils.httpResponse(404, "Not Found");
  }

  // trim and augment the profile
  profile = _.pick(profile, ["id", "name", "role_id"]);
  profile.is_admin = [1, 2].includes(profile.role_id);
  profile.is_liaison = profile.role_id === 3;
  let sql, rs;
  // determine if the user is a primary in any organization
  sql = "SELECT 1 FROM coc_user WHERE `primary` = 1 AND user_id = ?";
  rs = await utils.runRdsStatement(sql, [profile.id]);
  profile.is_coc_primary = rs.length > 0;
  // fetch the user's accessible modules
  sql = `
    SELECT modules.slug
      FROM modules
           INNER JOIN module_permissions ON modules.id = module_permissions.module_id
           INNER JOIN role_module_permissions ON module_permissions.id = role_module_permissions.module_permission_id
     WHERE role_module_permissions.role_id = ?
    UNION
    SELECT modules.slug
      FROM modules
           INNER JOIN module_permissions ON modules.id = module_permissions.module_id
           INNER JOIN user_module_permissions ON module_permissions.id = user_module_permissions.module_permission_id
     WHERE user_module_permissions.user_id = ?`;
  rs = await utils.runRdsStatement(sql, [profile.role_id, profile.id]);
  profile.modules = _.map(rs, "slug");

  return utils.httpResponse(200, profile);
};
