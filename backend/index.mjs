import fetch from "node-fetch";
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

  return utils.httpResponse(200, profile);
};
