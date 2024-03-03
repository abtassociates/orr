import mysql from "mysql2/promise";

/**
 * @param {string} statement
 * @param {any[]} params
 */
export const runRdsStatement = async (statement, params = []) => {
  const rds = await mysql.createConnection({
    host: process.env.LSA_HOST,
    database: process.env.LSA_DATABASE,
    user: process.env.LSA_USERNAME,
    password: process.env.LSA_PASSWORD,
    port: process.env.LSA_PORT,
  });
  const rs = await rds.query(statement, params);
  rds.end();

  return rs.shift();
};

/**
 * @param {number} statusCode
 * @param {any} body
 * @returns {object}
 */
export const httpResponse = (statusCode, body) => {
  const headers = { "Content-Type": statusCode === 200 ? "application/json" : "text/plain" };

  return {
    statusCode,
    headers,
    body: JSON.stringify(body),
  };
};
