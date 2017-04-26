#define STB_IMAGE_IMPLEMENTATION

#include <stdio.h>

#include "hitables.h"
#include "camera.h"
#include "ray.h"
#include "vec3.h"

int width = 200;
int height = 200;
int num_samples = 100;

HitableList hitables;

Vec3 color_ray(const Ray &ray) {
    int depth = 0;
    Ray current_ray = ray;
    Vec3 color = Vec3(1.0, 1.0, 1.0);
    
    while (depth < 50) {
        HitRecord hit_record = hitables.intersect(current_ray);

        if (hit_record.did_hit) {
            ScatterResult scatter_result = hit_record.material->scatter(current_ray, hit_record.position, hit_record.normal, hit_record.texture_coord);
            Vec3 emitted_light = hit_record.material->emitted(hit_record.position);

            if (scatter_result.did_scatter) {
                Vec3 ray_color = scatter_result.color;
                color = emitted_light + 
                        Vec3(color.x * scatter_result.color.x,
                             color.y * scatter_result.color.y,
                             color.z * scatter_result.color.z);
                current_ray = scatter_result.ray;
            }
            else {
                return Vec3(color.x * emitted_light.x, color.y * emitted_light.y, color.z * emitted_light.z);
            }
        }
        else {
            return Vec3(0.0, 0.0, 0.0);
        }

        depth++;
    }

    return Vec3(0.0, 0.0, 0.0);
}

void output_picture(Vec3 *colors) {
    FILE *img_file = fopen("image.ppm", "w");

    fprintf(img_file, "P3\n");
    fprintf(img_file, "%d %d\n", width, height);
    fprintf(img_file, "%d\n", 255);

    for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) {
            Vec3 color = colors[i * height + j]; 
            color.x = sqrt(color.x);
            color.y = sqrt(color.y);
            color.z = sqrt(color.z);
            color = Vec3::clamp(color, 0.0, 1.0); 

            int r = (int) (color.x * 255);
            int g = (int) (color.y * 255);
            int b = (int) (color.z * 255);
            fprintf(img_file, "%d %d %d\n", r, g, b);
        }
    }
}

int main() {
    Sphere s1(Vec3(0.0, -102.0, 0.0), 100.0);
    ConstantTexture t1(Vec3(0.8, 0.8, 0.8));
    ConstantTexture t2(Vec3(0.2, 0.2, 0.2));
    CheckeredTexture s1_texture(&t1, &t2);
    LambertianMaterial s1_material(&s1_texture);
    s1.material = &s1_material;

    Sphere s2(Vec3(0.0, 0.0, 0.0), 2.0);
    ConstantTexture s2_texture(Vec3(0.2, 0.8, 0.2));
    LambertianMaterial s2_material(&s2_texture);
    s2.material = &s2_material;

    Sphere s3(Vec3(3.0, -1.0, 0.0), 1.0);
    ConstantTexture s3_texture(Vec3(0.5, 0.5, 0.5));
    MetalMaterial s3_material(&s3_texture);
    s3.material = &s3_material;

    Sphere s4(Vec3(-3.0, -1.0, 0.0), 1.0);
    ImageTexture s4_texture("earth.jpg");
    LambertianMaterial s4_material(&s4_texture);
    s4.material = &s4_material;

    XYRect r1(Vec3(-300.0, -300.0, 0.0), Vec3(300.0, 300.0, 0.0));
    r1.material = &s2_material;
    TransformedHitable side1(&r1, Vec3(300.0, 0.0, 0.0), Vec3(0.0, -0.5 * M_PI, 0.0), Vec3(1.0, 1.0, 1.0));

    XYRect r2(Vec3(-300.0, -300.0, 0.0), Vec3(300.0, 300.0, 0.0));
    r2.material = &s2_material;
    TransformedHitable side2(&r2, Vec3(-300.0, 0.0, 0.0), Vec3(0.0, 0.5 * M_PI, 0.0), Vec3(1.0, 1.0, 1.0));

    XYRect r3(Vec3(-300.0, -300.0, 0.0), Vec3(300.0, 300.0, 0.0));
    r3.material = &s2_material;
    TransformedHitable side3(&r3, Vec3(0.0, 0.0, 300.0), Vec3(M_PI, 0.0, 0.0), Vec3(1.0, 1.0, 1.0));

    XYRect r4(Vec3(-300.0, -300.0, 0.0), Vec3(300.0, 300.0, 0.0));
    r4.material = &s2_material;
    TransformedHitable side4(&r4, Vec3(0.0, 300.0, 0.0), Vec3(0.5 * M_PI, 0.0, 0.0), Vec3(1.0, 1.0, 1.0));

    XYRect r5(Vec3(-300.0, -300.0, 0.0), Vec3(300.0, 300.0, 0.0));
    r5.material = &s2_material;
    TransformedHitable side5(&r5, Vec3(0.0, -300.0, 0.0), Vec3(-0.5 * M_PI, 0.0, 0.0), Vec3(1.0, 1.0, 1.0));

    ConstantTexture light_texture(Vec3(1.5, 1.5, 1.5));
    DiffuseLightMaterial light_material(&light_texture);
    XYRect light(Vec3(-200.0, -200.0, 0.0), Vec3(200.0, 200.0, 0.0));
    light.material = &light_material;
    TransformedHitable transformed_light(&light, Vec3(0.0, 299.0, 0.0), Vec3(0.5 * M_PI, 0.0, 0.0), Vec3(1.0, 1.0, 1.0));

    ConstantTexture box_texture(Vec3(0.8, 0.2, 0.2));
    LambertianMaterial box_material(&box_texture);
    Box box(Vec3(-100.0, -100.0, -100.0), Vec3(100.0, 100.0, 100.0));
    box.material = &box_material;
    TransformedHitable transformed_box(&box, Vec3(-100.0, -200.0, 100.0), Vec3(0.0, 0.2 * M_PI, 0.0), Vec3(1.0, 1.0, 1.0));

    //hitables.hitables.push_back(&s1);
    hitables.hitables.push_back(&side1);
    hitables.hitables.push_back(&side2);
    hitables.hitables.push_back(&side3);
    hitables.hitables.push_back(&side4);
    hitables.hitables.push_back(&side5);
    hitables.hitables.push_back(&transformed_light);
    hitables.hitables.push_back(&transformed_box);

    Camera camera(Vec3(0.0, 0.0, -800.0), Vec3(-300.0, -300.0, -305.0), Vec3(599.0, 0.0, 0.0), Vec3(0.0, 599.0, 0.0));
    Vec3 colors[width * height];

    for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) {
            colors[i * height + j] = Vec3(0.0, 0.0, 0.0);

            for (int k = 0; k < num_samples; k++) {
                float u = (float) (j + RAND(-0.5, 0.5)) / width;
                float v = 1.0 - ((float) (i + RAND(-0.5, 0.5)) / height);

                colors[i * height + j] = colors[i * height + j] + color_ray(camera.create_ray(u, v));
            }

            colors[i * height + j] = (1.0 / num_samples) * colors[i * height + j];
        }
    }

    output_picture(colors);
}
