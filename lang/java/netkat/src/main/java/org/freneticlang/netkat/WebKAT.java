package org.freneticlang.netkat;

import org.apache.http.client.fluent.*;
import org.freneticlang.netkat.*;
import org.apache.http.entity.*;
import org.json.*;

public class WebKAT
{
    public static void update(Policy policy) {
        try {
            /* TODO(jnf): Convert policies to JSON properly. */
            String json = "{ data : \"" + policy.toString() + "\", type : \"policy\"}";
            Request.Post("http://localhost:9000/update")
                .bodyString(json, ContentType.DEFAULT_TEXT)
                .execute().returnContent();
        } catch (Exception e) {
            System.out.println("Request failed: " + e.toString());
        }
    }

    public static JSONArray flowTable(long switchId) {
        try {
            String s = Request.Get("http://localhost:9000/" + Long.toString(switchId) + "/flow_table")
                .execute().returnContent().asString();
            return new JSONArray(s);
        } catch (Exception e) {
            System.out.println("Request failed: " + e.toString());
            return null;
        }
    }

    public static void main( String[] args )
    {
        Policy policy = new Sequence(new Filter(new Test("switch", "0")), 
                                     new Modification("port", "9"));
        update(policy);
        System.out.println(flowTable(0));
        System.out.println(flowTable(1));
    }
}
